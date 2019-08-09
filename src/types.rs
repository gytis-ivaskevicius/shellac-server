#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]

use super::parser;
use serde::{Deserialize, Serialize};
use std::{fmt, io, string::FromUtf8Error};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    FileNotUtf8(FromUtf8Error),
    WordOutOfRange(usize, usize),
    Parser(parser::Error),
}

impl From<io::Error> for Error {
    fn from(cause: io::Error) -> Self { Error::Io(cause) }
}

impl From<FromUtf8Error> for Error {
    fn from(cause: FromUtf8Error) -> Self { Error::FileNotUtf8(cause) }
}

impl From<parser::Error> for Error {
    fn from(cause: parser::Error) -> Self { Error::Parser(cause) }
}

impl std::error::Error for Error {}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io(cause) => write!(f, "io error: {}", cause),
            Error::WordOutOfRange(len, word) => write!(
                f,
                "the word {} can't be autocompleted because it is out of bound for argc = {}",
                word, len
            ),
            Error::FileNotUtf8(err) => write!(f, "The completion file is not valid utf8: {}", err),
            Error::Parser(err) => write!(f, "The completion file was invalid: {}", err),
        }
    }
}

#[derive(Default, Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct AutocompRequest {
    argv: Vec<String>,
    word: usize,
}

impl AutocompRequest {
    pub fn check(&self) -> Result<(), Error> {
        if self.word > self.argv.len() {
            Err(Error::WordOutOfRange(self.word, self.argv.len()))
        } else {
            Ok(())
        }
    }

    pub const fn argv(&self) -> &Vec<String> { &self.argv }

    pub const fn word(&self) -> usize { self.word }
}
