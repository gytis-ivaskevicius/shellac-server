use std::{
    io::{self, ErrorKind, Read},
    mem,
};

use super::{AutocompRequest, Error};

#[derive(Debug)]
pub struct ArgvCodec<R: Read> {
    reader: R,
}

impl<R: Read> ArgvCodec<R> {
    pub fn new(reader: R) -> Self { Self { reader } }
}

impl<R: Read> ArgvCodec<R> {
    pub fn decode(&mut self) -> Result<Option<AutocompRequest>, Error> {
        let mut argc = [0; 2];
        match self.reader.read(&mut argc)? {
            0 => return Ok(None),
            1 => {
                return Err(
                    io::Error::new(ErrorKind::UnexpectedEof, "failed to fill whole buffer").into()
                )
            }
            _ => (),
        }
        let argc = u16::from_be_bytes(argc) as usize;

        let mut word = [0; 2];
        self.reader.read_exact(&mut word)?;
        let word = u16::from_be_bytes(word) as usize;
        if word >= argc {
            return Err(Error::WordOutOfRange(argc, word));
        }

        std::iter::repeat_with(|| {
            let mut len = [0; mem::size_of::<usize>()];
            self.reader.read_exact(&mut len)?;
            let len = usize::from_be_bytes(len);

            let mut string = vec![0; len];
            self.reader.read_exact(&mut string)?;
            // Convert the bytes to a string and error if the bytes are not valid utf-8.
            String::from_utf8(string).map_err(Into::into)
        })
        .take(argc)
        .collect::<Result<_, _>>()
        .map(|argv| Some(AutocompRequest { argv, word }))
    }
}
