#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]

pub mod codec;
pub mod completion;
pub mod parser;

use serde::{Deserialize, Serialize};
use std::fmt;
use std::io;
use std::string::FromUtf8Error;

#[derive(Debug, Serialize)]
pub struct Result {
    choices: Vec<String>,
}

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    FileNotUtf8(FromUtf8Error),
    WordOutOfRange(usize, usize),
    DocOpt(String),
}

impl From<io::Error> for Error {
    fn from(cause: io::Error) -> Self {
        Error::Io(cause)
    }
}

impl From<FromUtf8Error> for Error {
    fn from(cause: FromUtf8Error) -> Self {
        Error::FileNotUtf8(cause)
    }
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
            Error::DocOpt(err) => write!(
                f,
                "The completion file contained an invalid docopt format: {}",
                err
            ),
        }
    }
}

#[derive(Default, Clone, Debug, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct AutocompRequest {
    argv: Vec<String>,
    word: usize,
}

impl AutocompRequest {
    pub const fn argv(&self) -> &Vec<String> {
        &self.argv
    }

    pub const fn word(&self) -> usize {
        self.word
    }
}
