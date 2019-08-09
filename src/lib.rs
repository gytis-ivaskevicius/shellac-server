pub mod completion;
mod errors;
pub mod parser;

// Codec definition
#[allow(dead_code)]
mod shellac_capnp {
    include!(concat!(env!("OUT_DIR"), "/shellac_capnp.rs"));
}

pub mod codec {
    use super::{shellac_capnp, Error};

    use std::{
        convert::TryInto,
        io::{self, BufRead, Write},
    };

    use capnp::{
        message::{self, ReaderOptions},
        serialize_packed as capn_serialize,
    };
    use serde::{Deserialize, Serialize};

    #[derive(Default, Clone, Debug, Hash, PartialEq, Eq, Deserialize)]
    pub struct AutocompRequest {
        pub argv: Vec<String>,
        pub word: u16,
    }

    #[derive(Default, Clone, Debug, Hash, PartialEq, Eq, Serialize)]
    pub struct Reply<'a> {
        pub choices: Vec<Choice<'a>>,
    }

    #[derive(Default, Clone, Debug, Hash, PartialEq, Eq, Serialize)]
    pub struct Choice<'a> {
        arg:         &'a str,
        description: &'a str,
    }

    impl<'a> Choice<'a> {
        pub const fn new(arg: &'a str, description: &'a str) -> Self { Choice { arg, description } }
    }

    pub fn write_reply<W: Write>(writer: &mut W, choices: Vec<String>) -> Result<(), io::Error> {
        let mut message = message::Builder::new_default();
        let reply = message.init_root::<shellac_capnp::response::Builder>();

        let mut reply_choices =
            reply.init_choices(choices.len().try_into().expect("Too many output choices"));
        for (i, choice) in choices.iter().enumerate() {
            let mut reply_choice = reply_choices.reborrow().get(i as u32);
            reply_choice.set_arg(choice);
            reply_choice.set_description("");
        }

        capn_serialize::write_message(writer, &message)
    }

    pub fn read_request<
        'a,
        R: BufRead + 'a,
        T,
        F: FnOnce(
            u16,
            capnp::text_list::Reader<'_>,
            &super::shellac_capnp::request::Reader,
        ) -> Result<T, Error>,
    >(
        reader: &mut R,
        f: F,
    ) -> Result<T, Error> {
        let request = capn_serialize::read_message(reader, ReaderOptions::default())?;
        let request = request.get_root::<super::shellac_capnp::request::Reader>()?;

        let argv = request.get_argv()?;
        let word = request.get_word();

        if word as u32 > argv.len() {
            Err(Error::WordOutOfRange(word, argv.len()))
        } else {
            f(word, argv, &request)
        }
    }

    // This is really ugly, but rust does not support impl Trait in trait bounds
    type Out<'a, E> = std::iter::Map<
        capnp::traits::ListIter<
            capnp::struct_list::Reader<'a, shellac_capnp::suggestion::Owned>,
            shellac_capnp::suggestion::Reader<'a>,
        >,
        fn(shellac_capnp::suggestion::Reader<'a>) -> Result<(&'a str, &'a str), E>,
    >;

    fn convert<'a, T: From<capnp::Error>>(
        choice: shellac_capnp::suggestion::Reader<'a>,
    ) -> Result<(&'a str, &'a str), T> {
        Ok((choice.get_arg()?, choice.get_description()?))
    }

    pub fn read_reply<R, T, E, F>(reader: &mut R, f: F) -> Result<T, E>
    where
        E: From<capnp::Error>,
        R: BufRead,
        F: FnOnce(Out<'_, E>) -> Result<T, E>,
    {
        let request = capnp::serialize_packed::read_message(reader, ReaderOptions::default())?;

        let choices =
            request.get_root::<super::shellac_capnp::response::Reader>()?.get_choices()?;
        f(choices.iter().map(convert))
    }

    pub fn write_request<W: Write>(
        writer: &mut W,
        input: &AutocompRequest,
    ) -> Result<(), io::Error> {
        let mut message = capnp::message::Builder::new_default();
        let mut output = message.init_root::<super::shellac_capnp::request::Builder>();
        output.set_word(input.word);

        let len = input.argv.len().try_into().expect("Too many output choices");
        let mut reply_argv = output.init_argv(len);
        for (i, arg) in input.argv.iter().enumerate() {
            reply_argv.reborrow().set(i as u32, arg);
        }

        capnp::serialize_packed::write_message(writer, &message)
    }
}

pub use errors::Error;
