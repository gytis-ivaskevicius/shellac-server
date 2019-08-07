use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
    convert::{TryFrom, TryInto},
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

use super::completion::{Argument, Operator, Sentinel, Step};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Definition {
    version:     u8,
    counters:    u8,
    arguments:   String,
    sections:    Definitions,
    definitions: BTreeMap<String, String>,
    desc:        BTreeMap<String, BTreeMap<String, String>>,
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
    fn alt[I]()(I) -> Alt<I::Range>
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + Default,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        sep_by1(sequence(), string("|"))
    }
}

fn repetition<I>() -> impl Parser<Input = I, Output = Repetition>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range + Default,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    choice!(char('+').map(|_| Repetition::Multiple), char('*').map(|_| Repetition::Any))
}

fn token<I>() -> impl Parser<Input = I, Output = Token<I::Range>>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range + Default,
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

fn sequence<I>() -> impl Parser<Input = I, Output = Vec<Token<I::Range>>>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range + Default,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    sep_by1(token(), string("=>"))
}

fn argument<I>() -> impl Parser<Input = I, Output = Argument<I::Range>>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range + Default,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (optional_word(), optional(many1((between(char('<'), char('>'), word()), optional_word()))))
        .map(|(literal, reference)| Argument::new(literal, reference))
}

fn optional_word<I>() -> impl Parser<Input = I, Output = I::Range>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range + Default,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    optional(word()).map(|word| word.unwrap_or_else(I::Range::default))
}

fn word<I>() -> impl Parser<Input = I, Output = I::Range>
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    take_while1(|c: char| c.is_alphanumeric() || c == '-' || c == '=' || c == '/')
}

fn arg_choice<'a, I: 'a>(
    descs: &'a [I::Range],
) -> impl Parser<Input = I, Output = (super::completion::Argument<I::Range>, super::completion::Choice)>
         + 'a
where
    I: Stream<Item = char> + RangeStream,
    I::Range: combine::stream::Range + combine::parser::combinator::StrLike + Default + Ord,
    I::Error: ParseError<I::Item, I::Range, I::Position>,
{
    (
        argument(),
        spaces(),
        optional(between(string("["), char(']'), word())),
        spaces(),
        optional(between(string("("), char(')'), sentinel())),
    )
        .map(move |(name, _, desc, _, sentinel)| {
            (
                name,
                super::completion::Choice::new(
                    desc.and_then(|desc| descs.binary_search(&desc).ok()),
                    sentinel,
                ),
            )
        })
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

impl<'a> TryFrom<&'a Definition> for super::completion::Definition<'a, &'a str> {
    type Error = regex::Error;

    fn try_from(def: &'a Definition) -> Result<Self, Self::Error> {
        if def.version != 0 {
            panic!("wrong version");
        }
        let (keys, descriptions): (Vec<_>, Vec<_>) =
            def.desc.iter().map(|(a, b)| (a.as_str(), b)).unzip();
        // eprintln!(
        // "{:#?}\n====",
        // def.sections
        // .iter()
        // .map(|(k, v)| (k, (v, keys.as_slice()).try_into().unwrap()))
        // .collect::<BTreeMap<_, super::completion::Arg<_>>>()
        // );
        let steps = alt().easy_parse(State::new(def.arguments.as_str())).unwrap().0;
        // eprintln!("{:#?}\n====", steps);
        let steps = resolve(steps, 0, &def.sections, keys.as_slice());
        eprintln!("{:#?}", steps);

        Ok(Self { num_counters: def.counters, descriptions, steps })
    }
}

impl<'a> TryFrom<&'a str> for super::completion::Argument<&'a str> {
    type Error = Errors<char, &'a str, SourcePosition>;

    fn try_from(arg: &'a str) -> Result<Self, Self::Error> {
        argument().easy_parse(State::new(arg)).map(|(arg, _)| arg)
    }
}

impl<'a> TryFrom<&'a str> for super::completion::Sentinel {
    type Error = Errors<char, &'a str, SourcePosition>;

    fn try_from(arg: &'a str) -> Result<Self, Self::Error> {
        sentinel().easy_parse(State::new(arg)).map(|(arg, _)| arg)
    }
}

impl<'a, 'b> TryFrom<(&'a Arg, &'b [&'a str])> for super::completion::Arg<&'a str> {
    type Error = regex::Error;

    fn try_from((arg, descs): (&'a Arg, &'b [&'a str])) -> Result<Self, Self::Error> {
        Ok(Self::new(
            arg.regex.as_ref().map(|regex| Regex::new(regex)).transpose()?,
            arg.choices
                .iter()
                .map(|choice| {
                    arg_choice(descs).easy_parse(State::new(choice.as_str())).map(|(arg, _)| arg)
                })
                .collect::<Result<_, _>>()
                .unwrap(),
        ))
    }
}

// TODO: Remove debug
fn resolve<'a, 'b>(
    mut alt: Alt<&'a str>,
    idx: u8,
    defs: &'a Definitions,
    descs: &'b [&'a str],
) -> Vec<Step<&'a str>> {
    // TODO: handle variations
    seq_to_vec(alt.pop().unwrap(), idx, defs, descs)
}

fn seq_to_vec<'a, 'b>(
    seq: Vec<Token<&'a str>>,
    idx: u8,
    defs: &'a Definitions,
    descs: &'b [&'a str],
) -> Vec<Step<&'a str>> {
    let mut steps = Vec::with_capacity(seq.len());
    for token in seq {
        match token {
            Token::Argument(arg) => steps.push(Step::Check(super::completion::Arg::new(
                None,
                vec![(arg, super::completion::Choice::new(None, None))].into_iter().collect(),
            ))),
            Token::Group(alt, Repetition::Once) => {
                steps.extend(resolve(alt, steps.len() as u8, defs, descs))
            }
            Token::Group(alt, Repetition::Any) => {
                let extra_steps = resolve(alt, steps.len() as u8 + 1, defs, descs);
                steps.push(Step::Split(idx + extra_steps.len() as u8 + 1));
                steps.extend(extra_steps);
                steps.push(Step::Split(idx + 1));
            }
            Token::Definition(def) => {
                steps.push(Step::Check((defs.get(def).unwrap(), descs).try_into().unwrap()))
            }
            a => {
                println!("{:?}", a);
                unimplemented!();
            }
        }
    }
    steps
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
                    super::super::completion::Choice::new(
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
