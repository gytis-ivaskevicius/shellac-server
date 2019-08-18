use serde_json::Deserializer;

use std::{
    env, fmt,
    io::{self, BufRead, BufReader, Read},
};

use shellac::codec;

#[derive(Debug)]
enum Error {
    CapnProto(capnp::Error),
    NotInSchema(capnp::NotInSchema),
    Serde(serde_json::Error),
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(cause: io::Error) -> Self { Error::Io(cause) }
}

impl From<capnp::NotInSchema> for Error {
    fn from(cause: capnp::NotInSchema) -> Self { Error::NotInSchema(cause) }
}

impl From<capnp::Error> for Error {
    fn from(cause: capnp::Error) -> Self { Error::CapnProto(cause) }
}

impl From<serde_json::Error> for Error {
    fn from(cause: serde_json::Error) -> Self { Error::Serde(cause) }
}

impl std::error::Error for Error {}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::CapnProto(e) => write!(f, "failure with cap'n proto: {}", e),
            Error::NotInSchema(e) => write!(f, "wrong cap'n proto protocol: {}", e),
            Error::Serde(e) => write!(f, "failed to translate to JSON: {}", e),
            Error::Io(e) => write!(f, "io error: {}", e),
        }
    }
}

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let result = match args.iter().map(String::as_str).collect::<Vec<_>>().as_slice() {
        [_, "encode"] => encode(io::stdin().lock()),
        [_, "encode", data] => encode(data.as_bytes()),
        [_, "decode"] => decode(io::stdin().lock()),
        _ => {
            eprintln!(
                "Usage:\n\tshellac-json encode [DATA]\n\tshellac-json decode\n\nDATA: string to \
                 encode. If absent, read from stdin"
            );
            std::process::exit(1);
        }
    };
    if let Err(err) = result {
        eprintln!("Fatal error: {}", err);
        std::process::exit(1);
    }
}

fn encode<R: Read>(reader: R) -> Result<(), Error> {
    for request in Deserializer::from_reader(BufReader::new(reader)).into_iter() {
        codec::write_request(&mut io::stdout().lock(), &request?)?;
    }
    Ok(())
}

fn decode<R: BufRead>(mut reader: R) -> Result<(), Error> {
    while !reader.fill_buf()?.is_empty() {
        codec::read_reply(&mut reader, |iter| {
            let reply = iter
                .map(|choice| {
                    choice.map(|(arg, description)| codec::Suggestion::new(arg, description))
                })
                .collect::<Result<Vec<_>, Error>>()?;
            serde_json::to_writer(&mut io::stdout().lock(), &reply).map_err(Error::from)
        })?;
    }
    Ok(())
}
