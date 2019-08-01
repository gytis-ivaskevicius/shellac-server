use std::collections::{BTreeMap, HashMap};

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Definition {
    version: u8,
    counters: u8,
    arguments: String,
    sections: HashMap<String, Arg>,
    definitions: HashMap<String, String>,
    desc: HashMap<u16, HashMap<String, String>>,
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

impl<'a> std::convert::TryFrom<&'a Definition> for super::completion::Definition<'a> {
    type Error = regex::Error;

    fn try_from(def: &'a Definition) -> Result<Self, Self::Error> {
        if def.version != 0 {
            panic!("wrong version");
        }
        let def_mapping = def
            .definitions
            .keys()
            .enumerate()
            .collect::<HashMap<_, _>>();
        Ok(Self {
            num_counters: def.counters,
            descriptions: def.definitions.values().map(String::as_str).collect(),
            steps: Vec::new(),
        })
    }
}
