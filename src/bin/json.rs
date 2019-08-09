use serde::{Deserialize, Serialize};
use serde_json::Deserializer;

use std::{
    convert::TryInto,
    env, fmt,
    io::{self, BufRead, BufReader, Read},
};

use shellac::codec;

#[derive(Debug)]
enum Error {
    CapnProto(capnp::Error),
    Serde(serde_json::Error),
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(cause: io::Error) -> Self { Error::Io(cause) }
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
            Error::Serde(e) => write!(f, "failed to translate to JSON: {}", e),
            Error::Io(e) => write!(f, "io error: {}", e),
        }
    }
}

#[derive(Default, Clone, Debug, Hash, PartialEq, Eq, Deserialize)]
struct AutocompRequest {
    pub argv: Vec<String>,
    pub word: u16,
}

#[derive(Default, Clone, Debug, Hash, PartialEq, Eq, Serialize)]
struct Reply<'a> {
    pub choices: Vec<Choice<'a>>,
}

#[derive(Default, Clone, Debug, Hash, PartialEq, Eq, Serialize)]
struct Choice<'a> {
    arg:         &'a str,
    description: &'a str,
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
    for request in Deserializer::from_reader(BufReader::new(reader)).into_iter::<AutocompRequest>()
    {
        let input = request?;
        let mut message = capnp::message::Builder::new_default();
        let mut output = message.init_root::<codec::request::Builder>();
        output.set_word(input.word);

        let len = input.argv.len().try_into().expect("Too many output choices");
        let mut reply_argv = output.init_argv(len);
        for (i, arg) in input.argv.iter().enumerate() {
            reply_argv.reborrow().set(i as u32, arg);
        }

        capnp::serialize_packed::write_message(&mut io::stdout().lock(), &message)?;
    }
    Ok(())
}

fn decode<R: BufRead>(mut reader: R) -> Result<(), Error> {
    while !reader.fill_buf()?.is_empty() {
        let request = match capnp::serialize_packed::read_message(
            &mut reader,
            capnp::message::ReaderOptions::default(),
        ) {
            Ok(r) => r,
            Err(ref err) if err.kind == capnp::ErrorKind::Disconnected => break,
            Err(err) => {
                eprintln!("Could not decode request: {}", err);
                std::process::exit(1);
            }
        };

        let request = request.get_root::<codec::response::Reader>()?;
        let choices = request.get_choices()?;
        let mut reply = Reply { choices: Vec::with_capacity(choices.len() as usize) };
        for choice in choices.iter() {
            reply.choices.push(Choice {
                arg:         choice.get_arg()?,
                description: choice.get_description()?,
            });
        }
        serde_json::to_writer(&mut io::stdout().lock(), &reply)?;
    }
    Ok(())
}
