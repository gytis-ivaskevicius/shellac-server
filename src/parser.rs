use std::{
    borrow::Borrow,
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    convert::TryFrom,
    fmt::{self, Debug, Display, Formatter},
};

use combine::{
    char::*,
    easy::Errors,
    range::*,
    stream::state::{SourcePosition, State},
    *,
};
use regex::Regex;
use serde::{Deserialize, Serialize};

use super::completion::{self, Argument, Operator, Sentinel, Step};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Definition {
    version:     u8,
    counters:    u8,
    arguments:   String,
    sections:    Definitions,
    definitions: BTreeMap<String, String>,
    desc:        BTreeMap<String, BTreeMap<String, String>>,
}

#[derive(Debug)]
pub enum Error {
    Regex(regex::Error),
    Parsing(Errors<char, String, combine::stream::state::SourcePosition>),
    TooMuchSteps(usize),
    UndefinedDescription(String),
}

impl std::error::Error for Error {}
impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Error::Regex(cause) => write!(f, "Invalid regex: {}", cause),
            Error::Parsing(err) => write!(f, "Invalid syntax: {}", err),
            Error::TooMuchSteps(size) => write!(
                f,
                "The number of steps for completion is too highh: {}. Please split the definition \
                 into a separate file",
                size
            ),
            Error::UndefinedDescription(desc) => {
                write!(f, "Reference to unknown description '{}'", desc)
            }
        }
    }
}

impl From<regex::Error> for Error {
    fn from(cause: regex::Error) -> Self { Error::Regex(cause) }
}

impl From<Errors<char, &str, combine::stream::state::SourcePosition>> for Error {
    fn from(cause: Errors<char, &str, combine::stream::state::SourcePosition>) -> Self {
        Error::Parsing(cause.map_range(ToOwned::to_owned))
    }
}

type Definitions = HashMap<String, Arg>;
type Alt<T> = Vec<Vec<Token<T>>>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum Token<T> {
    Group(Alt<T>, Repetition),
    Definition(T),
    Argument(Argument<T>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum Repetition {
    Optional, // ?
    Once,     // default
    Multiple, // +
    Any,      // *
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Arg {
    regex:   Option<String>,
    choices: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Choice {
    desc:  Option<String>,
    guard: Option<String>,
}

// Keep the parser because this is a recursive type
parser! {
    fn alt[I, O]()(I) -> Alt<O>
    where [
        O: From<I::Range> + Default,
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        sep_by1(sequence(), string("|"))
    }
}

fn repetition<I>() -> impl Parser<Input = I, Output = Repetition>
where
    I: Stream<Item = char>,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice!(char('+').map(|_| Repetition::Multiple), char('*').map(|_| Repetition::Any))
}

fn token<I, O: From<I::Range> + Default>() -> impl Parser<Input = I, Output = Token<O>>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    between(
        spaces(),
        spaces(),
        choice((
            (between(char('('), char(')'), alt()), optional(repetition())).map(
                |(group, repetition)| Token::Group(group, repetition.unwrap_or(Repetition::Once)),
            ),
            between(char('['), char(']'), alt())
                .map(|group| Token::Group(group, Repetition::Optional)),
            char('$').with(word()).map(Token::Definition),
            argument().map(Token::Argument),
        )),
    )
}

fn sequence<I, O: From<I::Range> + Default>() -> impl Parser<Input = I, Output = Vec<Token<O>>>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    sep_by1(token(), string("=>"))
}

fn argument<I, O: From<I::Range> + Default>() -> impl Parser<Input = I, Output = Argument<O>>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (optional_word(), optional(many1((between(char('<'), char('>'), word()), optional_word()))))
        .map(|(literal, reference)| Argument::new(literal, reference))
}

fn optional_word<I, O: From<I::Range> + Default>() -> impl Parser<Input = I, Output = O>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    optional(word()).map(|word| word.unwrap_or_else(O::default))
}

fn word<I, O: From<I::Range>>() -> impl Parser<Input = I, Output = O>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    take_while1(|c: char| c.is_alphanumeric() || c == '-' || c == '=' || c == '/').map(Into::into)
}

fn arg_choice<'a, 'b: 'a, I: 'a, O: 'b, T: 'b, S>(
    descs: &'a [S],
) -> impl Parser<Input = I, Output = (completion::Argument<O>, completion::Choice)> + 'a
where
    O: From<I::Range> + Default,
    S: Borrow<T> + 'b,
    T: Ord + ?Sized,
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range + Into<&'a T> + Ord + combine::parser::combinator::StrLike,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        argument(),
        spaces(),
        optional(between(char('['), char(']'), word::<_, I::Range>())).map(move |desc| {
            desc.and_then(|desc| descs.binary_search_by_key(&desc.into(), |s| s.borrow()).ok())
        }),
        spaces(),
        optional(between(char('('), char(')'), sentinel())),
    )
        .map(move |(name, _, desc, _, sentinel)| (name, completion::Choice::new(desc, sentinel)))
}

fn sentinel<I>() -> impl Parser<Input = I, Output = Sentinel>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range + combine::parser::combinator::StrLike,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        from_str(take_while1(|c: char| c.is_digit(10))),
        char(';'),
        optional(guard_check()),
        char(';'),
        optional(guard_assignment()),
    )
        .map(|(counter, _, test, _, change)| Sentinel::new(counter, test, change))
}

fn guard_check<I>() -> impl Parser<Input = I, Output = (Ordering, u8)>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range + combine::parser::combinator::StrLike,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        choice((
            char('=').map(|_| Ordering::Equal),
            char('>').map(|_| Ordering::Greater),
            char('<').map(|_| Ordering::Less),
        )),
        from_str(take_while1(|c: char| c.is_digit(10))),
    )
}

fn guard_assignment<I>() -> impl Parser<Input = I, Output = (Operator, u8)>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range + combine::parser::combinator::StrLike,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        choice((
            char('=').map(|_| Operator::Set),
            char('-').map(|_| Operator::Dec),
            char('+').map(|_| Operator::Inc),
        )),
        from_str(take_while1(|c: char| c.is_digit(10))),
    )
}

impl TryFrom<Definition> for completion::Definition<String> {
    type Error = Error;

    fn try_from(def: Definition) -> Result<Self, Self::Error> {
        if def.version != 0 {
            panic!("wrong version");
        }
        let (keys, descriptions): (Vec<_>, Vec<_>) = def.desc.into_iter().unzip();
        let steps = alt().easy_parse(State::new(def.arguments.as_str()))?.0;
        let mut steps = resolve(steps, 0, &def.sections, keys.as_slice())?;
        steps.push(Step::Match); // Last token is always match

        Ok(Self { num_counters: def.counters, descriptions, steps })
    }
}

impl<'a, O> TryFrom<&'a str> for completion::Argument<O>
where
    O: From<&'a str> + Default + Ord,
{
    type Error = Errors<char, &'a str, SourcePosition>;

    fn try_from(arg: &'a str) -> Result<Self, Self::Error> {
        argument().easy_parse(State::new(arg)).map(|(arg, _)| arg)
    }
}

impl<'a> TryFrom<&'a str> for completion::Sentinel {
    type Error = Errors<char, &'a str, SourcePosition>;

    fn try_from(arg: &'a str) -> Result<Self, Self::Error> {
        sentinel().easy_parse(State::new(arg)).map(|(arg, _)| arg)
    }
}

impl<'a, 'b: 'a, S: 'a, O: 'a> TryFrom<(&'a Arg, &'b [S])> for completion::Arg<O>
where
    O: From<&'a str> + Default + Ord,
    S: Borrow<str> + 'a,
{
    type Error = Error;

    fn try_from((arg, descs): (&'a Arg, &'b [S])) -> Result<Self, Self::Error> {
        Ok(Self::new(
            arg.regex.as_ref().map(|regex| Regex::new(regex)).transpose()?,
            arg.choices
                .iter()
                .map(|choice| {
                    arg_choice(descs).easy_parse(State::new(choice.as_str())).map(|(arg, _)| arg)
                })
                .collect::<Result<_, _>>()?,
        ))
    }
}

// TODO: Remove debug
fn resolve<'a, 'b, T, S>(
    mut alt: Alt<T>,
    mut idx: u8,
    defs: &'a Definitions,
    descs: &'b [S],
) -> Result<Vec<Step<T>>, Error>
where
    T: AsRef<str> + Ord + 'a,
    completion::Arg<T>: TryFrom<(&'a Arg, &'b [S])>,
    <completion::Arg<T> as TryFrom<(&'a Arg, &'b [S])>>::Error: Debug,
    Error: From<<completion::Arg<T> as TryFrom<(&'a Arg, &'b [S])>>::Error>,
{
    match alt.len() {
        0 => Ok(Vec::new()),
        // Since the length is one, the unwrap can't fail
        1 => seq_to_vec(alt.pop().unwrap(), idx, defs, descs),
        _ => {
            let mut steps = Vec::with_capacity(2 * alt.len());
            let mut alt = alt.into_iter();
            // Trick to avoid reparsing the entire string. This jump will be set at the end with the
            // correct length. Matching alternatives will jump to it and then match.
            let matching = idx + 1;
            idx += 2;
            steps.push(Step::Jump(idx));
            steps.push(Step::Jump(0));
            while let Some(seq) = alt.next() {
                let extra_steps = seq_to_vec(seq, idx, defs, descs)?;
                idx += u8::try_from(extra_steps.len())
                    .map_err(|_| Error::TooMuchSteps(extra_steps.len()))?
                    + 2;
                if alt.size_hint().0 != 0 {
                    steps.push(Step::Split(idx));
                }
                steps.extend(extra_steps);
                if alt.size_hint().0 != 0 {
                    steps.push(Step::Jump(matching))
                }
            }
            if let Some(Step::Jump(ref mut matching)) = steps.get_mut(1) {
                *matching = idx - 2;
            }
            Ok(steps)
        }
    }
}

fn seq_to_vec<'a, 'b, T, S>(
    seq: Vec<Token<T>>,
    idx: u8,
    defs: &'a Definitions,
    descs: &'b [S],
) -> Result<Vec<Step<T>>, Error>
where
    T: AsRef<str> + Ord + 'a,
    completion::Arg<T>: TryFrom<(&'a Arg, &'b [S])>,
    <completion::Arg<T> as TryFrom<(&'a Arg, &'b [S])>>::Error: Debug,
    Error: From<<completion::Arg<T> as TryFrom<(&'a Arg, &'b [S])>>::Error>,
{
    let mut steps = Vec::with_capacity(seq.len());
    for token in seq {
        let start =
            idx + u8::try_from(steps.len()).map_err(|_| Error::TooMuchSteps(steps.len()))?;
        match token {
            Token::Argument(arg) => steps.push(Step::Check(completion::Arg::new(
                None,
                vec![(arg, completion::Choice::new(None, None))].into_iter().collect(),
            ))),
            Token::Group(alt, Repetition::Once) => steps.extend(resolve(alt, start, defs, descs)?),
            Token::Group(alt, Repetition::Optional) => {
                let extra_steps = resolve(alt, start + 1, defs, descs)?;
                steps.push(Step::Split(start + extra_steps.len() as u8 + 1));
                steps.extend(extra_steps);
            }
            Token::Group(alt, Repetition::Any) => {
                let extra_steps = resolve(alt, start + 1, defs, descs)?;
                steps.push(Step::Split(start + extra_steps.len() as u8 + 2));
                steps.extend(extra_steps);
                steps.push(Step::Split(start + 1));
            }
            Token::Group(alt, Repetition::Multiple) => {
                let extra_steps = resolve(alt, start + 1, defs, descs)?;
                steps.extend(extra_steps);
                steps.push(Step::Split(start));
            }
            Token::Definition(def) => {
                let def = defs
                    .get(def.as_ref())
                    .ok_or_else(|| Error::UndefinedDescription(def.as_ref().to_string()))?;
                let step = completion::Arg::try_from((def, descs))?;
                steps.push(Step::Check(step))
            }
        }
    }
    Ok(steps)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_word() {
        assert_eq!(Ok(("ab/=--", "")), word().easy_parse("ab/=--"));
    }

    #[test]
    fn test_argument() {
        assert_eq!(
            Ok((Token::Argument(Argument::new("ab/=--", None)), "")),
            super::token().easy_parse("ab/=--")
        );
    }

    #[test]
    fn test_sequence() {
        assert_eq!(
            Ok((
                vec![Token::Argument(Argument::new("ab/=--", None)), Token::Definition("alfred")],
                ""
            )),
            sequence().easy_parse("     ab/=--        => $alfred")
        );
    }

    #[test]
    fn test_reference() {
        assert_eq!(
            Ok((
                vec![
                    vec![Token::Argument(Argument::new(
                        "sauce=",
                        Some(vec![("alfred", "="), ("name", "=bob")])
                    )),],
                    vec![
                        Token::Argument(Argument::new("a", None)),
                        Token::Argument(Argument::new("alfred", None))
                    ]
                ],
                ""
            )),
            alt().easy_parse("sauce=<alfred>=<name>=bob | a => alfred")
        );
    }

    #[test]
    fn test_sequences() {
        assert_eq!(
            Ok((
                vec![
                    vec![
                        Token::Argument(Argument::new("--/bob=", None)),
                        Token::Definition("alpha"),
                        Token::Argument(Argument::new("bob", None)),
                        Token::Argument(Argument::new("sauce=", Some(vec![("alfred", "")]))),
                    ],
                    vec![
                        Token::Argument(Argument::new("a", None)),
                        Token::Argument(Argument::new("alfred", None))
                    ]
                ],
                ""
            )),
            alt().easy_parse("--/bob=  =>   $alpha => bob   => sauce=<alfred> | a => alfred")
        );
    }

    #[test]
    fn test_group() {
        assert_eq!(
            Ok((
                vec![vec![
                    Token::Argument(Argument::new("--/bob=", None)),
                    Token::Group(vec![vec![Token::Definition("alpha")]], Repetition::Any,),
                    Token::Argument(Argument::new("bob", None)),
                    Token::Group(
                        vec![
                            vec![Token::Argument(Argument::new(
                                "sauce=",
                                Some(vec![("alfred", "")])
                            )),],
                            vec![
                                Token::Argument(Argument::new("a", None)),
                                Token::Argument(Argument::new("alfred", None))
                            ]
                        ],
                        Repetition::Once
                    )
                ]],
                ""
            )),
            alt()
                .easy_parse("--/bob=  =>   ( $alpha )* => bob   => (sauce=<alfred> | a => alfred)")
        );
    }

    #[test]
    fn test_sentinel() {
        assert_eq!(
            Ok((Sentinel::new(1, Some((Ordering::Equal, 2)), Some((Operator::Dec, 3))), "")),
            sentinel().easy_parse("1;=2;-3"),
        );
        assert_eq!(
            Ok((Sentinel::new(1, None, Some((Operator::Dec, 3))), "")),
            sentinel().easy_parse("1;;-3"),
        );
        assert_eq!(Ok((Sentinel::new(1, None, None), "")), sentinel().easy_parse("1;;"),);
    }

    #[test]
    fn test_arg_choice() {
        assert_eq!(
            Ok((
                (
                    Argument::new("f", None),
                    completion::Choice::new(
                        Some(1),
                        Some(Sentinel::new(
                            1,
                            Some((Ordering::Equal, 2)),
                            Some((Operator::Dec, 3))
                        ))
                    ),
                ),
                ""
            )),
            arg_choice(&["bob", "1"]).easy_parse("f [1] (1;=2;-3)"),
        );
        assert_eq!(
            Ok((Sentinel::new(1, None, Some((Operator::Dec, 3))), "")),
            sentinel().easy_parse("1;;-3"),
        );
        assert_eq!(Ok((Sentinel::new(1, None, None), "")), sentinel().easy_parse("1;;"),);
    }
}
