use std::error::Error;
use std::ffi::CString;
use std::fmt;
use std::fs::{self, File};
use std::path::PathBuf;

use bytes::BytesMut;
use memmap::Mmap;
use structopt::StructOpt;
use tokio::io;
use tokio::net::UnixListener;
use tokio::prelude::*;
use tokio_codec::FramedRead;

/// A Shell Agnostic Completion server
#[derive(StructOpt, Debug)]
#[structopt(name = "shellac-server")]
struct Opts {
    /// Activate debug mode
    #[structopt(short, long)]
    debug: bool,

    /// Verbose mode (-v, -vv, -vvv, etc.)
    #[structopt(short, long, parse(from_occurrences))]
    verbose: u8,

    /// Fallback completion (typically bash, zsh, fish)
    #[structopt(long)]
    fallback: Vec<String>,

    /// Completion for the shell's builtins
    #[structopt(long, parse(from_os_str))]
    shellcomp: Option<PathBuf>,

    /// Custom location for the socket
    #[structopt(short, long, parse(from_os_str))]
    socket: Option<PathBuf>,
}

#[derive(Default, Clone, Debug, Hash)]
pub struct ArgvCodec {
    read: usize,                 // The number of bytes read
    argc: usize,                 // Arg count
    word: Option<usize>,         // What word to autocomplete
    executable: Option<PathBuf>, // Path to the executable
    argv: Vec<CString>,          // The result
}

#[derive(Debug)]
pub enum ArgvCodecError {
    Io(io::Error),
    WordOutOfRange(usize, usize),
}

impl fmt::Display for ArgvCodecError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ArgvCodecError::Io(cause) => write!(f, "io error: {}", cause),
            ArgvCodecError::WordOutOfRange(len, word) => write!(
                f,
                "the word {} can't be autocompleted because it is out of bound for argc = {}",
                word, len
            ),
        }
    }
}

impl Error for ArgvCodecError {}

impl From<io::Error> for ArgvCodecError {
    fn from(err: io::Error) -> Self {
        ArgvCodecError::Io(err)
    }
}

#[derive(Default, Clone, Debug, Hash)]
pub struct AutocompRequest {
    argv: Vec<CString>,
    word: usize,
}

impl ArgvCodec {
    pub fn new() -> Self {
        Self::default()
    }
}

impl Drop for ArgvCodec {
    fn drop(&mut self) {
        if self.word.is_none() || self.argc != self.argv.len() {
            eprintln!(
                "Request dropped in an incoherent state: argc = {}, argv (partial) = {:?}",
                self.argc, self.argv
            );
        }
    }
}

impl tokio_codec::Decoder for ArgvCodec {
    type Item = AutocompRequest;
    type Error = ArgvCodecError;

    fn decode(&mut self, buf: &mut BytesMut) -> Result<Option<Self::Item>, Self::Error> {
        if self.argc == 0 {
            if buf.len() < 2 {
                return Ok(None);
            } else {
                self.argc = u16::from_be_bytes([buf[0], buf[1]]) as usize;
                buf.advance(2);
                self.argv.reserve(self.argc);
            }
        }
        if self.word.is_none() {
            if buf.len() < 2 {
                return Ok(None);
            } else {
                let word = u16::from_be_bytes([buf[0], buf[1]]) as usize;
                if word >= self.argc {
                    return Err(ArgvCodecError::WordOutOfRange(self.argc, word));
                }
                self.word = Some(word);
                buf.advance(2);
            }
        }

        if self.executable.is_none() {
            if let Some(mut offset) = buf[self.read..].iter().position(|&b| b == b'\0') {
                // The index of the '\n' is at the sum of the start position + the offset found.
                offset += self.read;
                let line = buf.split_to(offset + 1);

                // Drop the nul terminator
                let line = &line[..line.len() - 1];

                // Convert the bytes to a string and panic if the bytes are not valid utf-8.
                let line = PathBuf::new(line).unwrap(); // TODO: Proper error

                self.argv.push(line);
                self.read = 0;
                if self.argv.len() == self.argc {
                    self.argc = 0;
                    // Set the search start index back to 0.
                    return Ok(Some(AutocompRequest {
                        argv: std::mem::replace(&mut self.argv, Vec::new()),
                        word: self.word.unwrap(), // If the word is none, it is impossible that it compes here
                    }));
                }
            } else {

            return Ok(None);
        }


        while let Some(mut offset) = buf[self.read..].iter().position(|&b| b == b'\0') {
            // The index of the '\n' is at the sum of the start position + the offset found.
            offset += self.read;
            let line = buf.split_to(offset + 1);

            // Drop the nul terminator
            let line = &line[..line.len() - 1];

            // Convert the bytes to a string and panic if the bytes are not valid utf-8.
            let line = CString::new(line).expect("Parser error: a nul terminator was not caught");

            self.argv.push(line);
            self.read = 0;
            if self.argv.len() == self.argc {
                self.argc = 0;
                // Set the search start index back to 0.
                return Ok(Some(AutocompRequest {
                    argv: std::mem::replace(&mut self.argv, Vec::new()),
                    word: self.word.unwrap(), // If the word is none, it is impossible that it compes here
                }));
            }
        }

        // Ok(None) signifies that more data is needed to produce a full frame.
        Ok(None)
    }
}

fn main() {
    let opts = Opts::from_args();

    let path = opts
        .socket
        .unwrap_or_else(|| PathBuf::from("/run/user/1000/shellac.sock"));

    let listener = UnixListener::bind(&path).unwrap_or_else(|_| {
        fs::remove_file(&path).unwrap();
        UnixListener::bind(&path).unwrap()
    });

    let task = listener
        .incoming()
        .map_err(|err| {
            // Handle error by printing to STDOUT.
            println!("accept error = {}", err);
        })
        .for_each(|socket| {
            println!("socket connected!");
            let (reader, _writer) = socket.split();
            // copy bytes from the reader into the writer
            FramedRead::new(reader, ArgvCodec::new())
                .and_then(|request| {
                    let mut file = File::open(PathBuf::new("demo").join(&request.argv[0]))?;

                    let mut contents = Vec::new();
                    file.read_to_end(&mut contents)?;

                    let mmap = unsafe { Mmap::map(&file)? };
                    Ok((request, mmap))
                })
                .for_each(|result| {
                    println!("Received result {:?}", result);
                    Ok(())
                })
                .then(|request| match request {
                    Ok(()) => {
                        println!("Socket closed");
                        Ok(())
                    }
                    Err(err) => {
                        println!("Could not execute request: {}", err);
                        Ok(())
                    }
                })
        });

    tokio::run(task)
}
