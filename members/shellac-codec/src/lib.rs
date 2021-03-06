use capnp::{
    message::{self, ReaderOptions},
    serialize_packed as capn_serialize,
};
use serde::{Deserialize, Serialize};
use shellac_capnp::{request::Reader as RequestReader, response::Builder as ResponseBuilder};
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

/// Parsing error
#[derive(Debug)]
pub enum Error {
    /// The word is out of bound for the given argv length
    WordOutOfRange { word: u16, argv_len: u32 },
    /// Incorrect cap'n proto format
    Capnp(capnp::Error),
    /// The requested variable is not in the schema
    NotInSchema(capnp::NotInSchema),
}

impl From<capnp::NotInSchema> for Error {
    fn from(cause: capnp::NotInSchema) -> Self { Error::NotInSchema(cause) }
}

impl From<capnp::Error> for Error {
    fn from(cause: capnp::Error) -> Self { Error::Capnp(cause) }
}

impl std::error::Error for Error {}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::WordOutOfRange { word, argv_len } => write!(
                f,
                "the word {} can't be autocompleted because it is out of bound for argc = {}",
                word, argv_len
            ),
            Error::Capnp(e) => write!(f, "{}", e),
            Error::NotInSchema(e) => write!(f, "{}", e),
        }
    }
}

/// A ShellAC autocompletion request
#[derive(Default, Clone, Debug, Hash, PartialEq, Eq, Deserialize)]
pub struct AutocompRequest {
    argv: Vec<String>,
    word: u16,
}

/// A ShellAC autocompletion reply. The type parameter is to allow borrowed or owned string types
#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize)]
pub struct Reply<T> {
    pub choices: Vec<Suggestion<T>>,
}

/// One of two kind of suggestion type
#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize)]
pub enum SuggestionType<T> {
    /// A literal suggestion (ex: `-b` after git checkout)
    Literal(T),
    /// A command to execute while removing the provided prefix (ex: if the user typed `git
    /// checkout mybr`, execute `git branch --no-color --list 'mybr*'` with prefix `mybr`)
    Command { command: Vec<T>, prefix: T },
}

/// A single autocompletion suggestion
#[derive(Clone, Debug, Hash, PartialEq, Eq, Serialize)]
pub struct Suggestion<T> {
    /// The suggestion
    suggestion: SuggestionType<T>,
    /// It's description. May be provided to the user to indicate the effect of a given suggestion
    /// (ex: `-b` after `git checkout` could have the description `create a new branch`)
    description: T,
}

impl AutocompRequest {
    /// Generate a new ShellAC autocompletion request. Panics if the word is greater than the argv
    /// length.
    pub fn new(argv: Vec<String>, word: u16) -> Self {
        if word as usize >= argv.len() {
            eprintln!(
                "Word {} is out of bound for argv '{:?}' in ShellAC autocompletion request",
                word, argv
            );
            panic!();
        }
        Self { argv, word }
    }
}

impl<T> Suggestion<T> {
    /// Generate a new suggestion to encode
    pub const fn new(suggestion: SuggestionType<T>, description: T) -> Self {
        Self { suggestion, description }
    }

    /// Get the associated description
    pub const fn description(&self) -> &T { &self.description }

    /// Get the associated suggestion
    pub const fn suggestion(&self) -> &SuggestionType<T> { &self.suggestion }
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
                let command = cmd.get_args()?.iter().collect::<Result<Vec<_>, _>>()?;
                SuggestionType::Command { command, prefix }
            }
        },
        choice.get_description()?,
    ))
}

/// Read a ShellAC Server reply without (necessarily) collecting.
///
/// ```rust
/// use std::io::{self, BufReader};
///
/// shellac::read_reply::<_, _, shellac::Error, _>(&mut BufReader::new(io::stdin()), |suggestions| {
///     for (suggestion, description) in suggestions.map(Result::unwrap) {
///         println!("Suggestion: '{:?}' ({})", suggestion, description);
///     }
///     Ok(())
/// });
/// ```
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

/// Write a request to a listening ShellAC server.
///
/// ```rust
/// use std::io;
/// use shellac::AutocompRequest;
///
/// shellac::write_request(
///     &mut io::stdout(),
///     &AutocompRequest::new(vec!["git".into(), "checkout".into(), "myb".into()], 2)
/// ).unwrap();
/// ```
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

/// Send a reply like the `ShellAC` server would to the other end of a the Writer, where a client
/// would listen. You should not need this
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

/// Read a `ShellAC` Request without allocating. You should not need this
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
