use std::collections::{BTreeMap, HashMap};

use combine::{char::*, stream::state::State, *};
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
    fn sequences[I]()(I) -> Vec<Vec<Argument>>
    where [
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        sep_by1(sequence(), string("|"))
    }
}

parser! {
    fn sequence[I]()(I) -> Vec<Argument>
    where [
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        sep_by1(argument(), string("=>"))
    }
}

parser! {
    fn argument[I]()(I) -> Argument
    where [
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        between(spaces(), spaces(), choice(
            (
                char('$').with(word()).map(Argument::Definition),
                attempt((
                    optional(word()),
                    between(char('<'), char('>'), word())
                ))
                    .map(|(prefix, reference)| Argument::Reference(prefix.unwrap_or_else(String::new), reference)),
                word().map(Argument::Literal)
            )
        ))
    }
}

parser! {
    fn word[I]()(I) -> String
    where [
        I: Stream<Item = char>,
        I::Error: ParseError<I::Item, I::Range, I::Position>,
    ] {
        many1(letter().or(char('-')).or(char('=')).or(char('/')))
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum Argument {
    Literal(String),
    Reference(String, String),
    Definition(String),
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
