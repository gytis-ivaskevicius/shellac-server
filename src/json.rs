#[allow(dead_code)]
mod completion;
mod parser;
mod types;

// Codec definition
#[allow(dead_code)]
mod shellac_capnp {
    include!(concat!(env!("OUT_DIR"), "/shellac_capnp.rs"));
}

use serde::{Deserialize, Serialize};
use serde_json::Deserializer;

use std::{
    convert::TryInto,
    env,
    io::{self, BufRead, BufReader, Read},
};

use types::Error;

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
    match args.iter().map(String::as_str).collect::<Vec<_>>().as_slice() {
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
    }
}

fn encode<R: Read>(reader: R) {
    for request in Deserializer::from_reader(BufReader::new(reader)).into_iter::<AutocompRequest>()
    {
        let input = request.unwrap();
        let mut message = capnp::message::Builder::new_default();
        let mut output = message.init_root::<shellac_capnp::request::Builder>();
        output.set_word(input.word);

        let len = input.argv.len().try_into().expect("Too many output choices");
        let mut reply_argv = output.init_argv(len);
        for (i, arg) in input.argv.iter().enumerate() {
            reply_argv.reborrow().set(i as u32, arg);
        }

        capnp::serialize::write_message(&mut io::stdout().lock(), &message).unwrap();
    }
}

fn decode<R: BufRead>(mut reader: R) {
    loop {
        if reader.fill_buf().unwrap().len() == 0 {
            break;
        }

        let request = match capnp::serialize::read_message(
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
        let request = request.get_root::<shellac_capnp::response::Reader>().unwrap();
        let mut reply =
            Reply { choices: Vec::with_capacity(request.get_choices().unwrap().len() as usize) };
        for choice in request.get_choices().unwrap().iter() {
            reply.choices.push(Choice {
                arg:         choice.get_arg().unwrap(),
                description: choice.get_description().unwrap(),
            });
        }
        serde_json::to_writer(&mut io::stdout().lock(), &reply).unwrap();
    }
}
