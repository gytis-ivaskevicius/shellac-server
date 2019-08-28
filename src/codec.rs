use std::{
    convert::TryInto,
    fmt,
    io::{self, BufRead, Write},
};

// Codec definition
#[allow(dead_code)]
mod shellac_capnp {
    include!(concat!(env!("OUT_DIR"), "/shellac_capnp.rs"));
}
pub use shellac_capnp::request::Reader;

use capnp::{
    message::{self, ReaderOptions},
    serialize_packed as capn_serialize,
};
use serde::{Deserialize, Serialize};

#[derive(Debug)]
pub enum Error {
    WordOutOfRange(u16, u32),
    Capnp(capnp::Error),
}

impl From<capnp::Error> for Error {
    fn from(cause: capnp::Error) -> Self { Error::Capnp(cause) }
}

impl std::error::Error for Error {}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::WordOutOfRange(word, len) => write!(
                f,
                "the word {} can't be autocompleted because it is out of bound for argc = {}",
                word, len
            ),
            Error::Capnp(e) => write!(f, "{}", e),
        }
    }
}

#[derive(Default, Clone, Debug, Hash, PartialEq, Eq, Deserialize)]
pub struct AutocompRequest {
    pub argv: Vec<String>,
    pub word: u16,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize)]
pub struct Reply<T> {
    pub choices: Vec<Suggestion<T>>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize)]
pub enum SuggestionType<T> {
    Literal(T),
    Command(Vec<T>, T),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize)]
pub struct Suggestion<T> {
    arg:         SuggestionType<T>,
    description: T,
}

impl<T> Suggestion<T> {
    pub const fn new(arg: SuggestionType<T>, description: T) -> Self {
        Suggestion { arg, description }
    }
}

pub fn write_reply<W: Write>(
    writer: &mut W,
    choices: Vec<Suggestion<String>>,
) -> Result<(), io::Error> {
    let mut message = message::Builder::new_default();
    let reply = message.init_root::<shellac_capnp::response::Builder>();

    let mut reply_choices =
        reply.init_choices(choices.len().try_into().expect("Too many output choices"));
    for (i, choice) in choices.iter().enumerate() {
        let mut reply_choice = reply_choices.reborrow().get(i as u32);
        match &choice.arg {
            SuggestionType::Literal(lit) => reply_choice.reborrow().init_arg().set_literal(&lit),
            SuggestionType::Command(cmd, prefix) => {
                let mut builder = reply_choice.reborrow().init_arg().init_command();
                builder.set_prefix(prefix);
                let mut args = builder.init_args(cmd.len() as u32);
                for (i, arg) in cmd.iter().enumerate() {
                    args.set(i as u32, arg);
                }
            }
        }
        reply_choice.set_description(&choice.description);
    }

    capn_serialize::write_message(writer, &message)
}

pub fn read_request<
    'a,
    R: BufRead + 'a,
    T,
    E: From<Error>,
    F: FnOnce(u16, capnp::text_list::Reader<'_>, &shellac_capnp::request::Reader) -> Result<T, E>,
>(
    reader: &mut R,
    f: F,
) -> Result<T, E> {
    let request =
        capn_serialize::read_message(reader, ReaderOptions::default()).map_err(Into::into)?;
    let request = request.get_root::<shellac_capnp::request::Reader>().map_err(Into::into)?;

    let argv = request.get_argv().map_err(Into::into)?;
    let word = request.get_word();

    if u32::from(word) > argv.len() {
        Err(Error::WordOutOfRange(word, argv.len()).into())
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
    fn(shellac_capnp::suggestion::Reader<'a>) -> Result<(SuggestionType<&'a str>, &'a str), E>,
>;

fn convert<T: From<capnp::Error> + From<capnp::NotInSchema>>(
    choice: shellac_capnp::suggestion::Reader,
) -> Result<(SuggestionType<&str>, &str), T> {
    Ok((
        match choice.get_arg().which()? {
            shellac_capnp::suggestion::arg::Which::Literal(lit) => SuggestionType::Literal(lit?),
            shellac_capnp::suggestion::arg::Which::Command(cmd) => {
                let cmd = cmd?;
                let prefix = cmd.get_prefix()?;
                let args = cmd.get_args()?.iter().collect::<Result<Vec<_>, _>>()?;
                SuggestionType::Command(args, prefix)
            }
        },
        choice.get_description()?,
    ))
}

pub fn read_reply<R, T, E, F>(reader: &mut R, f: F) -> Result<T, E>
where
    E: From<capnp::Error> + From<capnp::NotInSchema>,
    R: BufRead,
    F: FnOnce(Out<'_, E>) -> Result<T, E>,
{
    let request = capnp::serialize_packed::read_message(reader, ReaderOptions::default())?;

    let choices = request.get_root::<shellac_capnp::response::Reader>()?.get_choices()?;
    f(choices.iter().map(convert))
}

pub fn write_request<W: Write>(writer: &mut W, input: &AutocompRequest) -> Result<(), io::Error> {
    let mut message = capnp::message::Builder::new_default();
    let mut output = message.init_root::<shellac_capnp::request::Builder>();
    output.set_word(input.word);

    let len = input.argv.len().try_into().expect("Too many output choices");
    let mut reply_argv = output.init_argv(len);
    for (i, arg) in input.argv.iter().enumerate() {
        reply_argv.reborrow().set(i as u32, arg);
    }

    capnp::serialize_packed::write_message(writer, &message)
}
