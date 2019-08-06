use std::collections::{BTreeMap, HashMap};

use combine::{char::*, range::*, stream::state::State, *};
use regex::Regex;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Definition {
    version: u8,  // Done
    counters: u8, // Done
    arguments: String,
    sections: HashMap<String, Arg>,
    definitions: BTreeMap<String, String>,            // Done
    desc: BTreeMap<String, BTreeMap<String, String>>, // Done
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
    fn sequences[I]()(I) -> Vec<Vec<Argument<I::Range>>>
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + Default,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        sep_by1(sequence(), string("|"))
    }
}

parser! {
    fn sequence[I]()(I) -> Vec<Argument<I::Range>>
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + Default,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        sep_by1(argument(), string(" "))
    }
}

parser! {
    fn argument[I]()(I) -> Argument<I::Range>
    where [
        I: Stream<Item = char> + RangeStream,
        I::Range: combine::stream::Range + Default,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        between(spaces(), spaces(), choice(
            (
                char('$').with(word()).map(Argument::Definition),
                attempt((
                    optional(word()),
                    between(char('<'), char('>'), word())
                ))
                    .map(|(prefix, reference)| Argument::Reference(prefix.unwrap_or_else(I::Range::default), reference)),
                word().map(Argument::Literal)
            )
        ))
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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum Argument<T> {
    Literal(T),
    Reference(T, T),
    Definition(T),
}

impl std::convert::TryFrom<Definition> for super::completion::Definition {
    type Error = regex::Error;

    fn try_from(def: Definition) -> Result<Self, Self::Error> {
        if def.version != 0 {
            panic!("wrong version");
        }
        let (keys, values): (Vec<_>, Vec<_>) = def.desc.into_iter().unzip();
        eprintln!("{:#?}\n====", def.arguments);

        match sequences().easy_parse(State::new(
            "--/bob=  =>   $alpha => bob   => sauce=<alfred> | a => alfred",
        )) {
            Ok((value, _remaining_input)) => println!("{:#?}", value),
            Err(err) => println!("{}", err),
        }

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
            Ok((Argument::Literal("ab/=--"), "")),
            argument().easy_parse("ab/=--")
        );
    }

    #[test]
    fn test_sequence() {
        assert_eq!(
            Ok((
                vec![Argument::Literal("ab/=--"), Argument::Definition("alfred")],
                ""
            )),
            sequence().easy_parse("     ab/=--        => $alfred")
        );
    }

    #[test]
    fn test_sequences() {
        assert_eq!(
            Ok((
                vec![
                    vec![
                        Argument::Literal("--/bob="),
                        Argument::Definition("alpha"),
                        Argument::Literal("bob"),
                        Argument::Reference("sauce=", "alfred"),
                    ],
                    vec![Argument::Literal("a"), Argument::Literal("alfred")]
                ],
                ""
            )),
            sequences().easy_parse("--/bob=  =>   $alpha => bob   => sauce=<alfred> | a => alfred")
        );
    }
}
