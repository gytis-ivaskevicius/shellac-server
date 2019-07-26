use shellac_server::{codec::ArgvCodec, completion};

use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use structopt::StructOpt;
use tokio::fs::File;
use tokio::net::UnixListener;
use tokio::prelude::future::result;
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

fn get_comp_file(argv0: &str) -> io::Result<PathBuf> {
    // WONTFIX: Does not work on windows
    let path = &Path::new(argv0);
    let path = Path::new("completion").join(&path.file_name().unwrap());
    // TODO: check if this is not a file
    Ok(path.with_extension("shellac"))
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
            println!("Could not accept socket request {}", err);
        })
        .for_each(|socket| {
            println!("socket connected!");
            let (reader, _writer) = socket.split();
            // copy bytes from the reader into the writer
            FramedRead::new(reader, ArgvCodec::new())
                .and_then(|request| {
                    result(get_comp_file(&request.argv()[0]))
                        .and_then(File::open)
                        .map_err(Into::into)
                        .and_then(|file| completion::complete(file, request))
                })
                .for_each(|result| {
                    println!("Received result {:?}", result);
                    Ok(())
                })
                .then(|result| {
                    match result {
                        Ok(()) => println!("Socket closed"),
                        Err(err) => println!("Could not execute request: {}", err),
                    }
                    Ok(())
                })
        });

    tokio::run(task)
}
