#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]

use super::parser;
use std::{fmt, io, string::FromUtf8Error, sync::PoisonError};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    FileNotUtf8(FromUtf8Error),
    WordOutOfRange(usize, usize),
    Parser(parser::Error),
    Codec(capnp::Error),
    Cache,
}

impl From<io::Error> for Error {
    fn from(cause: io::Error) -> Self { Error::Io(cause) }
}

impl From<FromUtf8Error> for Error {
    fn from(cause: FromUtf8Error) -> Self { Error::FileNotUtf8(cause) }
}

impl From<capnp::Error> for Error {
    fn from(cause: capnp::Error) -> Self { Error::Codec(cause) }
}

impl From<parser::Error> for Error {
    fn from(cause: parser::Error) -> Self { Error::Parser(cause) }
}

impl<T> From<PoisonError<T>> for Error {
    fn from(_cause: PoisonError<T>) -> Self { Error::Cache }
}

impl std::error::Error for Error {}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::Io(e) => write!(f, "io error: {}", e),
            Error::WordOutOfRange(len, word) => write!(
                f,
                "the word {} can't be autocompleted because it is out of bound for argc = {}",
                word, len
            ),
            Error::FileNotUtf8(e) => write!(f, "The completion file is not valid utf8: {}", e),
            Error::Parser(e) => write!(f, "The completion file was invalid: {}", e),
            Error::Codec(e) => write!(f, "Request decoding failed: {}", e),
            Error::Cache => write!(f, "Another thread crashed, corrupting the cache"),
        }
    }
}
