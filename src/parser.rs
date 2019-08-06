use std::collections::{BTreeMap, HashMap};

use combine::{char::*, range::*, stream::state::State, *};
use regex::Regex;
use serde::{Deserialize, Serialize};

use super::completion::Sentinel;

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
struct Argument<T> {
    literal: T,
    reference: Option<Vec<(T, T)>>,
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
    choices: BTreeMap<String, Choice>,
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
            char('+'),
            char('*'),
            char('?'),
        ))).map(|result| {
            match result {
                None => Repetition::Once,
                Some('+') => Repetition::Multiple,
                Some('*') => Repetition::Any,
                Some('?') => Repetition::Optional,
                _ => unreachable!(),
            }
        })
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
            )
                .map(|(literal, reference)| Argument::new(literal, reference))
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
    fn sentinel[I]()(I) -> Sentinel
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + combine::parser::combinator::StrLike,
        I::Error: ParseError<I::Item, I::Range, I::Position> + From<std::num::ParseIntError>,
    ] {
        between(
            char('('),
            char(')'),
            (
                from_str(take_while1(|c: char| c.is_digit(10))),
                char(';'),
                optional(string("=1")),
                char(';'),
                optional(string("+1"))
            ).map(|(counter, _, test, _, change)| Sentinel::new(counter, None, None))
        )
    }
}

impl<T> Argument<T> {
    pub const fn new(literal: T, reference: Option<Vec<(T, T)>>) -> Self {
        Self { literal, reference }
    }

    pub const fn literal(literal: T) -> Self {
        Self {
            literal,
            reference: None,
        }
    }
}

impl std::convert::TryFrom<Definition> for super::completion::Definition {
    type Error = regex::Error;

    fn try_from(def: Definition) -> Result<Self, Self::Error> {
        if def.version != 0 {
            panic!("wrong version");
        }
        let (keys, values): (Vec<_>, Vec<_>) = def.desc.into_iter().unzip();
        eprintln!("{:#?}\n====", def.sections);
        eprintln!(
            "{:#?}",
            alt().easy_parse(State::new(def.arguments.as_str()))
        );

        Ok(Self {
            num_counters: def.counters,
            descriptions: values,
            steps: Vec::new(),
        })
    }
}

impl std::convert::TryFrom<Arg> for super::completion::Arg {
    type Error = regex::Error;

    fn try_from(arg: Arg) -> Result<Self, Self::Error> {
        Ok(Self::new(
            arg.regex.map(|regex| Regex::new(&regex)).transpose()?,
            BTreeMap::default(),
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
            Ok((Token::Argument(Argument::literal("ab/=--")), "")),
            super::token().easy_parse("ab/=--")
        );
    }

    #[test]
    fn test_sequence() {
        assert_eq!(
            Ok((
                vec![
                    Token::Argument(Argument::literal("ab/=--")),
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
                        Token::Argument(Argument::literal("a")),
                        Token::Argument(Argument::literal("alfred"))
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
                        Token::Argument(Argument::literal("--/bob=")),
                        Token::Definition("alpha"),
                        Token::Argument(Argument::literal("bob")),
                        Token::Argument(Argument::new("sauce=", Some(vec![("alfred", "")]))),
                    ],
                    vec![
                        Token::Argument(Argument::literal("a")),
                        Token::Argument(Argument::literal("alfred"))
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
                    Token::Argument(Argument::literal("--/bob=")),
                    Token::Group(vec![vec![Token::Definition("alpha")]], Repetition::Any,),
                    Token::Argument(Argument::literal("bob")),
                    Token::Group(
                        vec![
                            vec![Token::Argument(Argument::new(
                                "sauce=",
                                Some(vec![("alfred", "")])
                            )),],
                            vec![
                                Token::Argument(Argument::literal("a")),
                                Token::Argument(Argument::literal("alfred"))
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
}
