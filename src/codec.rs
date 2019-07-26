use std::io::{self, BufRead, BufReader, BufWriter, Read};
use std::os::unix::net::UnixStream;

use super::AutocompRequest;
use super::Error;

#[derive(Debug)]
pub struct ArgvCodec<'a> {
    reader: BufReader<&'a UnixStream>,
    writer: BufWriter<&'a UnixStream>,
}

impl<'a> ArgvCodec<'a> {
    pub fn new(socket: &'a UnixStream) -> Self {
        Self {
            reader: BufReader::new(socket),
            writer: BufWriter::new(socket),
        }
    }
}

impl<'a> ArgvCodec<'a> {
    pub fn decode(&mut self) -> Result<Option<AutocompRequest>, Error> {
        if self.reader.fill_buf()?.len() == 0 {
            return Ok(None);
        }
        let mut argc = [0u8; 2];
        self.reader.read_exact(&mut argc)?;
        let argc = u16::from_be_bytes(argc) as usize;

        let mut word = [0u8; 2];
        self.reader.read_exact(&mut word)?;
        let word = u16::from_be_bytes(word) as usize;
        if word >= argc {
            return Err(Error::WordOutOfRange(argc, word));
        }

        std::iter::repeat_with(|| {
            let mut string = Vec::with_capacity(32);
            if self.reader.fill_buf()?.len() == 0 {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    format!("Protocol failure, wrong argc ({})", argc),
                )
                .into());
            }
            self.reader.read_until(b'\0', &mut string)?;
            if string.pop() != Some(b'\0') {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "Protocol failure, an argument was improperly null terminated",
                )
                .into());
            }
            // Convert the bytes to a string and error if the bytes are not valid utf-8.
            String::from_utf8(string).map_err(Into::into)
        })
        .take(argc)
        .collect::<Result<_, _>>()
        .map(|argv| Some(AutocompRequest { argv, word }))
    }

    pub fn encode(&mut self, _result: super::Result) -> Result<(), Error> {
        Ok(())
    }
}
