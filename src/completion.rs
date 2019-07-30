use regex::Regex;
use retain_mut::RetainMut;

use std::fs::File;
use std::io::BufReader;
use std::io::Read;

use super::AutocompRequest;
use super::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Step {
    Check(Arg),
    Jump(u8),
    Split(u8),
    Match,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition {
    steps: Vec<Step>,
    num_counters: u8,
}

#[derive(Debug, Clone)]
pub struct Arg {
    regex: Option<Regex>,
    choices: Vec<Choice>,
}

impl Eq for Arg {}

impl PartialEq for Arg {
    fn eq(&self, other: &Self) -> bool {
        self.choices.eq(&other.choices)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Choice {
    description: Option<String>,
    typ: ChoiceType,
    sentinel: Option<String>, // TODO: better representation for sentinels
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChoiceType {
    Literal(String),
    Reference(String),
}

#[derive(Debug)]
pub enum ChoiceResolver {
    Literal(std::iter::Once<String>),
    Reference(std::vec::IntoIter<String>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VMSearcher<'a> {
    def: &'a Definition,
    stack: Vec<Searcher>,
    args: &'a AutocompRequest,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Searcher {
    counters: Vec<u8>,
    step: u8,
    completion: Option<u8>,
}

impl Iterator for ChoiceResolver {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ChoiceResolver::Literal(iter) => iter.next(),
            ChoiceResolver::Reference(iter) => iter.next(),
        }
    }
}

impl ChoiceResolver {
    pub fn new<'a>(choice: &'a Choice) -> Self {
        match &choice.typ {
            ChoiceType::Literal(lit) => ChoiceResolver::Literal(std::iter::once(lit.to_string())),
            ChoiceType::Reference(reference) => {
                ChoiceResolver::Reference(vec![reference.to_string()].into_iter())
            }
        }
    }
}

impl<'a> IntoIterator for &'a Choice {
    type Item = String;
    type IntoIter = ChoiceResolver;

    fn into_iter(self) -> ChoiceResolver {
        ChoiceResolver::new(self)
    }
}

impl Searcher {
    pub fn new(num_counters: u8, step: u8) -> Self {
        Self {
            counters: vec![0; num_counters as usize],
            step,
            completion: None,
        }
    }

    pub fn step(&mut self) {
        self.step += 1;
    }
}

impl<'a> VMSearcher<'a> {
    fn new(def: &'a Definition, args: &'a AutocompRequest) -> Self {
        Self {
            def,
            stack: vec![Searcher::new(def.num_counters, 0)],
            args,
        }
    }

    fn choices(mut self) -> Result<Vec<String>, Error> {
        for (i, arg) in self.args.argv().iter().enumerate().skip(1) {
            // Advance to the next argument
            while self.stack.iter().any(|searcher| {
                if let Step::Check(..) | Step::Match = self.def.steps[searcher.step as usize] {
                    false
                } else {
                    true
                }
            }) {
                let stack_len = self.stack.len();
                for j in 0..stack_len {
                    let searcher = &mut self.stack[j];

                    match self.def.steps[searcher.step as usize] {
                        Step::Jump(i) => searcher.step = i,
                        Step::Split(i) => {
                            let mut clone = searcher.clone();
                            clone.step = i;
                            searcher.step();
                            self.stack.push(clone);
                        }
                        _ => (),
                    }
                }
            }

            if i as usize == self.args.word {
                let def = &self.def;

                self.stack.retain_mut(|searcher| {
                    if searcher.step as usize >= def.steps.len() {
                        return false;
                    }

                    match &def.steps[searcher.step as usize] {
                        Step::Check(_def) => {
                            searcher.completion = Some(searcher.step);
                            searcher.step();
                            true
                        }
                        Step::Match => false,
                        _ => unreachable!(),
                    }
                })
            } else {
                let def = &self.def;
                self.stack
                    .retain_mut(|searcher| match def.steps.get(searcher.step as usize) {
                        Some(Step::Check(ref arg_def)) => {
                            searcher.step();
                            arg_def
                                .regex
                                .as_ref()
                                .filter(|regex| !regex.is_match(arg))
                                .is_none()
                                && (searcher.step as usize) < def.steps.len()
                        }
                        None | Some(Step::Match) => false,
                        _ => unreachable!(),
                    });
            }
        }

        let VMSearcher {
            mut stack,
            args,
            def,
        } = self;
        stack.sort_unstable_by(|a, b| match (a.completion, b.completion) {
            (Some(a), Some(b)) => a.cmp(&b),
            (Some(_), None) => std::cmp::Ordering::Greater,
            (None, Some(_)) => std::cmp::Ordering::Less,
            (None, None) => std::cmp::Ordering::Equal,
        });

        if let Some(result) = stack
            .into_iter()
            .filter_map(|searcher| searcher.completion)
            .map(|idx| &def.steps[idx as usize])
            //                .filter(|def| {
            //                    def.regex
            //                        .as_ref()
            //                        .filter(|regex| !regex.is_match(&args.argv()[args.word]))
            //                        .is_none()
            //                })
            .next()
        {
            let result = if let Step::Check(check) = result {
                check
            } else {
                unreachable!()
            };
            Ok(result.choices.iter().flat_map(|choice| choice).collect())
        } else {
            Ok(Vec::new())
        }
    }
}

impl Definition {
    fn new<T: AsRef<str>>(_def: &T) -> Result<Self, regex::Error> {
        Ok(Definition {
            num_counters: 0,
            steps: vec![
                Step::Split(3),
                Step::Check(Arg {
                    regex: Some(Regex::new(r"^-([a-zA-Z])+$|^--([a-zA-Z_\-=]{2,})$")?),
                    choices: vec![Choice {
                        description: Some("hello".to_string()),
                        typ: ChoiceType::Literal("l".to_owned()),
                        sentinel: None,
                    }],
                }),
                Step::Split(1),
                Step::Split(5),
                Step::Check(Arg {
                    regex: Some(Regex::new(r"^--$")?),
                    choices: vec![],
                }),
                Step::Check(Arg {
                    regex: None,
                    choices: vec![Choice {
                        description: None,
                        typ: ChoiceType::Reference("file".to_string()),
                        sentinel: None,
                    }],
                }),
                Step::Check(Arg {
                    regex: None,
                    choices: vec![Choice {
                        description: None,
                        typ: ChoiceType::Reference("file".to_owned()),
                        sentinel: None,
                    }],
                }),
                Step::Match,
            ],
        })
    }
}

pub struct Completer {
    file: BufReader<File>,
    request: AutocompRequest,
}

pub fn complete(file: File, request: AutocompRequest) -> Result<super::Result, Error> {
    Completer::new(file, request).complete()
}

impl Completer {
    pub fn new(file: File, request: AutocompRequest) -> Self {
        Self {
            request,
            file: BufReader::new(file),
        }
    }

    pub fn complete(mut self) -> Result<super::Result, Error> {
        let mut content = String::with_capacity(1024);

        self.file.read_to_string(&mut content)?;

        let def = Definition::new(&content.trim()).unwrap();
        Ok(super::Result {
            choices: VMSearcher::new(&def, &self.request).choices().unwrap(),
        })
    }
}
