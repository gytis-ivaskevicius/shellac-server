use std::cmp::Ordering;
use std::collections::{BTreeMap, HashMap};
use std::convert::{TryFrom, TryInto};

use combine::{
    char::*,
    easy::Errors,
    range::*,
    stream::state::{SourcePosition, State},
    *,
};
use regex::Regex;
use serde::{Deserialize, Serialize};

use super::completion::{Argument, Operator, Sentinel};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Definition {
    version: u8,  // Done
    counters: u8, // Done
    arguments: String,
    sections: HashMap<String, Arg>,
    definitions: BTreeMap<String, String>,            // Done
    desc: BTreeMap<String, BTreeMap<String, String>>, // Done
}

type Alt<T> = Vec<Vec<Token<T>>>;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum Token<T> {
    Group(Alt<T>, Repetition),
    Optional(Alt<T>),
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
    regex: Option<String>,
    choices: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Choice {
    desc: Option<String>,
    guard: Option<String>,
}

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

parser! {
    fn repetition[I]()(I) -> Repetition
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + Default,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        optional(choice((
            char('+').map(|_| Repetition::Multiple),
            char('*').map(|_| Repetition::Any),
            char('?').map(|_| Repetition::Optional),
        ))).map(|result| result.unwrap_or(Repetition::Once))
    }
}

parser! {
    fn token[I]()(I) -> Token<I::Range>
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + Default,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        between(spaces(), spaces(), choice((
            (
                between(char('('), char(')'), alt()),
                repetition()
            ).map(|(group, repetition)| Token::Group(group, repetition)),
            between(char('['), char(']'), alt()).map(Token::Optional),
            char('$').with(word()).map(Token::Definition),
            argument().map(Token::Argument),
        )))
    }
}

parser! {
    fn sequence[I]()(I) -> Vec<Token<I::Range>>
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + Default,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        sep_by1(token(), string("=>"))
    }
}

parser! {
    fn argument[I]()(I) -> Argument<I::Range>
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + Default,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        (
            optional_word(),
            optional(many1((between(char('<'), char('>'), word()), optional_word())))
        ).map(|(literal, reference)| Argument::new(literal, reference))
    }
}

parser! {
    fn optional_word[I]()(I) -> I::Range
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + Default,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        optional(word()).map(|word| word.unwrap_or_else(I::Range::default))
    }
}

parser! {
    fn word[I]()(I) -> I::Range
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        take_while1(|c: char| c.is_alphanumeric() || c == '-' || c == '=' || c == '/')
    }
}

parser! {
    fn arg_choice['a, I](descs: &'a [I::Range])(I) -> (super::completion::Argument<I::Range>, super::completion::Choice)
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + combine::parser::combinator::StrLike + Default + Ord,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        (
            argument(),
            optional(between(string(" ["), char(']'), word())),
            optional(between(string(" ("), char(')'), sentinel())),
        ).map(|(name, desc, sentinel)| {
            (
                name,
                super::completion::Choice::new(
                    desc.and_then(|desc| descs.binary_search(&desc).ok()),
                    sentinel,
                ),
            )
        })
    }
}

parser! {
    fn sentinel[I]()(I) -> Sentinel
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + combine::parser::combinator::StrLike,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        (
            from_str(take_while1(|c: char| c.is_digit(10))),
            char(';'),
            optional(guard_check()),
            char(';'),
            optional(guard_assignment())
        ).map(|(counter, _, test, _, change)| Sentinel::new(counter, test, change))
    }
}

parser! {
    fn guard_check[I]()(I) -> (Ordering, u8)
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + combine::parser::combinator::StrLike,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        (
            choice((
                char('=').map(|_| Ordering::Equal),
                char('>').map(|_| Ordering::Greater),
                char('<').map(|_| Ordering::Less)
            )),
            from_str(take_while1(|c: char| c.is_digit(10))),
        )
    }
}

parser! {
    fn guard_assignment[I]()(I) -> (Operator, u8)
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + combine::parser::combinator::StrLike,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        (
            choice((
                char('=').map(|_| Operator::Set),
                char('-').map(|_| Operator::Dec),
                char('+').map(|_| Operator::Inc)
            )),
            from_str(take_while1(|c: char| c.is_digit(10))),
        )
    }
}

impl<T: Ord> TryFrom<Definition> for super::completion::Definition<T> {
    type Error = regex::Error;

    fn try_from(def: Definition) -> Result<Self, Self::Error> {
        if def.version != 0 {
            panic!("wrong version");
        }
        let (keys, descriptions): (Vec<_>, Vec<_>) = def.desc.into_iter().unzip();
        let keys = keys.iter().map(String::as_str).collect::<Vec<_>>();
        /* eprintln!(
            "{:#?}\n====",
            def.sections
                .iter()
                .map(|(k, v)| (k, (v, keys.as_slice()).try_into().unwrap()))
                .collect::<BTreeMap<_, super::completion::Arg<_>>>()
        );
        eprintln!(
            "{:#?}",
            alt().easy_parse(State::new(def.arguments.as_str()))
        );*/
        let steps = Vec::new();

        Ok(Self {
            num_counters: def.counters,
            descriptions,
            steps,
        })
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

impl<'a> TryFrom<(&'a Arg, &'a [&'a str])> for super::completion::Arg<&'a str> {
    type Error = regex::Error;

    fn try_from((arg, descs): (&'a Arg, &'a [&'a str])) -> Result<Self, Self::Error> {
        Ok(Self::new(
            arg.regex
                .as_ref()
                .map(|regex| Regex::new(regex))
                .transpose()?,
            arg.choices
                .iter()
                .map(|choice| {
                    arg_choice(descs)
                        .easy_parse(State::new(choice.as_str()))
                        .map(|(arg, _)| arg)
                })
                .collect::<Result<_, _>>()
                .unwrap(),
        ))
    }
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
                vec![
                    Token::Argument(Argument::new("ab/=--", None)),
                    Token::Definition("alfred")
                ],
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
            Ok((
                Sentinel::new(1, Some((Ordering::Equal, 2)), Some((Operator::Dec, 3))),
                ""
            )),
            sentinel().easy_parse("1;=2;-3"),
        );
        assert_eq!(
            Ok((Sentinel::new(1, None, Some((Operator::Dec, 3))), "")),
            sentinel().easy_parse("1;;-3"),
        );
        assert_eq!(
            Ok((Sentinel::new(1, None, None), "")),
            sentinel().easy_parse("1;;"),
        );
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
        assert_eq!(
            Ok((Sentinel::new(1, None, None), "")),
            sentinel().easy_parse("1;;"),
        );
    }
}
