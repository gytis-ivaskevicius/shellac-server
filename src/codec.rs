use std::io::{self, BufRead, Read};

use super::AutocompRequest;
use super::Error;

#[derive(Debug)]
pub struct ArgvCodec<R: Read + BufRead> {
    reader: R,
}

impl<R: Read + BufRead> ArgvCodec<R> {
    pub fn new(reader: R) -> Self {
        Self { reader }
    }
}

impl<R: Read + BufRead> ArgvCodec<R> {
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
}
