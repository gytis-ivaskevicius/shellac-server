use regex::Regex;
use retain_mut::RetainMut;

use std::collections::BTreeMap;
use std::fs::File;
use std::io::BufReader;
use std::io::Read;
use std::process::Command;
use std::process::Stdio;

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

#[derive(Debug, Clone, Default)]
pub struct Arg {
    regex: Option<Regex>,
    choices: BTreeMap<ChoiceType, Choice>,
}

impl Eq for Arg {}

impl PartialEq for Arg {
    fn eq(&self, other: &Self) -> bool {
        self.choices.eq(&other.choices)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Choice {
    description: Option<String>,
    sentinel: Option<String>, // TODO: better representation for sentinels
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ChoiceType {
    Literal(String),
    Reference(String, String),
}

#[derive(Debug)]
pub enum ChoiceResolver<T> {
    Literal(std::iter::Once<T>),
    Reference(std::vec::IntoIter<T>),
    None,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VMSearcher<'a> {
    def: &'a Definition,
    stack: Vec<Searcher>,
    args: &'a AutocompRequest,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Searcher {
    counters: Vec<u8>,
    step: u8,
    completion: Option<u8>,
}

impl<T> Iterator for ChoiceResolver<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ChoiceResolver::Literal(iter) => iter.next(),
            ChoiceResolver::Reference(iter) => iter.next(),
            ChoiceResolver::None => None,
        }
    }
}

impl ChoiceType {
    pub fn as_str(&self) -> &str {
        match self {
            ChoiceType::Literal(lit) => lit,
            ChoiceType::Reference(prefix, _) => prefix,
        }
    }

    pub fn resolve(&self, start: &str) -> ChoiceResolver<String> {
        match self {
            ChoiceType::Literal(lit) if lit.starts_with(start) => {
                ChoiceResolver::Literal(std::iter::once(lit.to_string()))
            }
            ChoiceType::Reference(prefix, reference) => {
                if prefix.starts_with(start) {
                    ChoiceResolver::Literal(std::iter::once(prefix.to_string()))
                } else if start.starts_with(prefix) {
                    if reference == "file" {
                        let file_start = start.trim_start_matches(prefix);
                        let out = Command::new("ls")
                            .arg("-1")
                            .stdout(Stdio::piped())
                            .spawn()
                            .unwrap()
                            .wait_with_output()
                            .unwrap();
                        ChoiceResolver::Reference(
                            String::from_utf8(out.stdout)
                                .unwrap()
                                .lines()
                                .filter(|line| line.starts_with(file_start))
                                .map(|line| format!("{}{}", prefix, line))
                                .collect::<Vec<_>>()
                                .into_iter(),
                        )
                    } else {
                        ChoiceResolver::Literal(std::iter::once(format!(
                            "{}<{}>",
                            prefix, reference
                        )))
                    }
                } else {
                    ChoiceResolver::None
                }
            }
            _ => ChoiceResolver::None,
        }
    }
}

impl std::str::FromStr for ChoiceType {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let mut parts = input.splitn(2, '<');
        let start = parts.next().unwrap();
        if let Some(desc) = parts.next() {
            let (desc, end) = desc.split_at(desc.len() - 1);
            if end == ">" {
                Ok(ChoiceType::Reference(start.into(), desc.into()))
            } else {
                Err(())
            }
        } else {
            Ok(ChoiceType::Literal(input.into()))
        }
    }
}

impl std::cmp::Ord for ChoiceType {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}
impl std::cmp::PartialOrd for ChoiceType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_str().partial_cmp(other.as_str())
    }
}

impl Arg {
    pub fn resolve(&self, results: &mut Vec<String>, arg: &str) {
        if let Some(regex) = &self.regex {
            let len = arg.len();
            let mut test = arg.to_string();

            for option in self.choices.keys() {
                test.truncate(len);
                test.push_str(option.as_str());

                // TODO: this does not check all capture groups
                if let Some(captures) = regex.captures(&test) {
                    if captures.iter().filter_map(|x| x).any(|capture| {
                        self.choices
                            .keys()
                            .any(|key| key.as_str() == capture.as_str())
                    }) {
                        results.push(test.clone());
                    }
                }
            }
            if !results.is_empty() {
                return;
            }
            if let Some(captures) = regex.captures(arg) {
                results.extend(captures.iter().filter_map(|capture| {
                    let capture = capture?;
                    if let Some(choice) = self
                        .choices
                        .keys()
                        .find(|option| option.as_str().starts_with(capture.as_str()))
                    {
                        Some(format!(
                            "{}{}{}",
                            &arg[..capture.start()],
                            choice.as_str(),
                            &arg[capture.end()..]
                        ))
                    } else {
                        None
                    }
                }));
            }
        } else {
            results.extend(self.choices.keys().flat_map(|choice| choice.resolve(arg)))
        }
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
                        Step::Check(_arg_def) => {
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
                            (searcher.step as usize) < def.steps.len()
                                && (if let Some(regex) = &arg_def.regex {
                                    regex.is_match(arg)
                                } else {
                                    true
                                })
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
        let arg = &args.argv()[args.word];

        let mut results = Vec::with_capacity(20);
        stack.sort_unstable_by_key(|searcher| searcher.step);
        stack.dedup_by_key(|searcher| searcher.step);
        for searcher in stack {
            if let Some(completion) = searcher.completion {
                if let Step::Check(check) = &def.steps[completion as usize] {
                    check.resolve(&mut results, arg);
                }
            }
        }
        Ok(results)
    }
}

impl Definition {
    fn new<T: AsRef<str>>(_def: &T) -> Result<Self, regex::Error> {
        Ok(Self {
            num_counters: 0,
            steps: vec![
                Step::Split(3),
                Step::Check(Arg {
                    regex: Some(Regex::new(r"^-([a-zA-Z])+$|^--([a-zA-Z_\-=]{2,})$")?),
                    choices: vec![
                        ("long".parse().unwrap(), Choice::default()),
                        ("lower".parse().unwrap(), Choice::default()),
                        ("l".parse().unwrap(), Choice::default()),
                        ("a".parse().unwrap(), Choice::default()),
                    ]
                    .into_iter()
                    .collect(),
                }),
                Step::Split(1),
                Step::Split(5),
                Step::Check(Arg {
                    regex: None,
                    choices: vec![("--".parse().unwrap(), Choice::default())]
                        .into_iter()
                        .collect(),
                }),
                Step::Check(Arg {
                    regex: None,
                    choices: vec![("<file>".parse().unwrap(), Choice::default())]
                        .into_iter()
                        .collect(),
                }),
                Step::Check(Arg {
                    regex: None,
                    choices: vec![("<file>".parse().unwrap(), Choice::default())]
                        .into_iter()
                        .collect(),
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
