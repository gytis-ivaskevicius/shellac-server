use super::{RequestReader, ResponseBuilder};
use capnp::{
    message::{self, ReaderOptions},
    serialize_packed as capn_serialize,
};
use shellac::{Error, Suggestion, SuggestionType};
use std::{
    convert::TryInto,
    io::{self, BufRead, Write},
};

/// Send a reply like the `ShellAC` server would to the other end of a the Writer, where a client
/// would listen.
pub fn write_reply<'a, W: Write, T: AsRef<str> + 'a, I: IntoIterator<Item = &'a Suggestion<T>>>(
    writer: &mut W,
    choices: I,
) -> Result<(), io::Error>
where
    I::IntoIter: ExactSizeIterator,
{
    let mut message = message::Builder::new_default();
    let reply = message.init_root::<ResponseBuilder>();

    let choices = choices.into_iter();
    let mut reply_choices =
        reply.init_choices(choices.len().try_into().expect("Too many output choices"));
    for (i, choice) in choices.enumerate() {
        let mut reply_choice = reply_choices.reborrow().get(i as u32);
        match choice.suggestion() {
            SuggestionType::Literal(lit) => {
                reply_choice.reborrow().init_arg().set_literal(lit.as_ref())
            }
            SuggestionType::Command { command, prefix } => {
                let mut builder = reply_choice.reborrow().init_arg().init_command();
                builder.set_prefix(prefix.as_ref());
                let mut args = builder.init_args(command.len() as u32);
                for (i, arg) in command.iter().enumerate() {
                    args.set(i as u32, arg.as_ref());
                }
            }
        }
        reply_choice.set_description(choice.description().as_ref());
    }

    capn_serialize::write_message(writer, &message)
}

/// Read a `ShellAC` Request without allocating.
pub fn read_request<
    'a,
    R: BufRead + 'a,
    T,
    E: From<Error>,
    F: FnOnce(u16, capnp::text_list::Reader<'_>, &RequestReader) -> Result<T, E>,
>(
    reader: &mut R,
    f: F,
) -> Result<T, E> {
    let request =
        capn_serialize::read_message(reader, ReaderOptions::default()).map_err(Into::into)?;
    let request = request.get_root::<RequestReader>().map_err(Into::into)?;

    let argv = request.get_argv().map_err(Into::into)?;
    let word = request.get_word();

    if u32::from(word) > argv.len() {
        Err(Error::WordOutOfRange { word, argv_len: argv.len() }.into())
    } else {
        f(word, argv, &request)
    }
}
