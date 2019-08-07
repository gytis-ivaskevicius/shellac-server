use regex::Regex;
use retain_mut::RetainMut;

use std::{
    cmp::{Ord, Ordering, PartialOrd},
    collections::BTreeMap,
};
// use std::process::Command;
// use std::process::Stdio;

use super::{AutocompRequest, Error};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Step<T: Ord> {
    Check(Arg<T>),
    Jump(u8),
    Split(u8),
    Match,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition<'a, T: Ord> {
    pub steps:        Vec<Step<T>>,
    pub num_counters: u8,
    pub descriptions: Vec<&'a BTreeMap<String, String>>, /* A HashMap is clearer, but a vec is
                                                          * faster */
}

#[derive(Debug, Clone, Default)]
pub struct Arg<T: Ord> {
    regex: Option<Regex>,
    // TODO: Is this the best datastructure? Can we access variables directly? If not, maybe a
    // simple vec with a binary search would be better
    choices: BTreeMap<Argument<T>, Choice>,
}

impl<T: Eq + Ord> Eq for Arg<T> {}

impl<T: PartialEq + Ord> PartialEq for Arg<T> {
    fn eq(&self, other: &Self) -> bool { self.choices.eq(&other.choices) }
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Choice {
    description: Option<usize>,
    sentinel:    Option<Sentinel>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Sentinel {
    counter:    u8,
    check:      Option<(Ordering, u8)>,
    assignment: Option<(Operator, u8)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    Inc,
    Dec,
    Set,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Argument<T> {
    literal:   T,
    reference: Option<Vec<(T, T)>>,
}

#[derive(Debug)]
pub enum ChoiceResolver<T> {
    Literal(std::iter::Once<T>),
    Reference(std::vec::IntoIter<T>),
    None,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VMSearcher<'a, T: Ord> {
    def:   &'a Definition<'a, T>,
    stack: Vec<Searcher>,
    args:  &'a AutocompRequest,
}

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub struct Searcher {
    counters:   Vec<u8>,
    step:       u8,
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

impl<T> Argument<T> {
    pub const fn literal(&self) -> &T { &self.literal }

    pub const fn new(literal: T, reference: Option<Vec<(T, T)>>) -> Self {
        Self { literal, reference }
    }
}

impl<'a> Argument<&'a str> {
    pub fn to_owned(&self) -> Argument<String> {
        Argument {
            literal:   self.literal.to_owned(),
            reference: self
                .reference
                .as_ref()
                .map(|refs| refs.iter().map(|(r, f)| (r.to_string(), f.to_string())).collect()),
        }
    }
}

impl<T: AsRef<str> + ToString + std::fmt::Debug> Argument<T> {
    pub fn resolve(&self, start: &str) -> ChoiceResolver<String> {
        match &self.reference {
            None if self.literal.as_ref().starts_with(start) => {
                ChoiceResolver::Literal(std::iter::once(self.literal.to_string()))
            }
            Some(reference) => {
                if self.literal.as_ref().starts_with(start) {
                    ChoiceResolver::Literal(std::iter::once(self.literal.to_string()))
                } else if start.starts_with(self.literal.as_ref()) {
                    // if reference == "file" {
                    // let file_start = start.trim_start_matches(self.literal);
                    // let out = Command::new("ls")
                    // .arg("-1")
                    // .stdout(Stdio::piped())
                    // .spawn()
                    // .unwrap()
                    // .wait_with_output()
                    // .unwrap();
                    // ChoiceResolver::Reference(
                    // String::from_utf8(out.stdout)
                    // .unwrap()
                    // .lines()
                    // .filter(|line| line.starts_with(file_start))
                    // .map(|line| format!("{}{}", prefix, line))
                    // .collect::<Vec<_>>()
                    // .into_iter(),
                    // )
                    // } else {
                    ChoiceResolver::Literal(std::iter::once(format!(
                        "{}<{:?}>",
                        self.literal.as_ref(),
                        reference
                    )))
                // }
                } else {
                    ChoiceResolver::None
                }
            }
            _ => ChoiceResolver::None,
        }
    }
}

impl Choice {
    pub const fn new(description: Option<usize>, sentinel: Option<Sentinel>) -> Self {
        Self { description, sentinel }
    }

    pub fn check(&self, counters: &[u8]) -> bool {
        self.sentinel
            .as_ref()
            .map_or(true, |sentinel| sentinel.check(counters[sentinel.counter as usize]))
    }
}

impl<T: Ord> Ord for Argument<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.literal().cmp(other.literal()) }
}

impl<T: PartialOrd> PartialOrd for Argument<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.literal().partial_cmp(other.literal())
    }
}

impl<T: Ord> Arg<T> {
    pub fn new(regex: Option<Regex>, choices: BTreeMap<Argument<T>, Choice>) -> Self {
        Self { regex, choices }
    }
}

impl<T: Ord + AsRef<str> + std::fmt::Debug + ToString> Arg<T> {
    pub fn resolve(&self, results: &mut Vec<String>, arg: &str, counters: &[u8]) {
        if let Some(regex) = &self.regex {
            let len = arg.len();
            let mut test = arg.to_string();

            for (option, _) in self.choices.iter().filter(|(_, desc)| desc.check(counters)) {
                test.truncate(len);
                test.push_str(option.literal().as_ref());

                // TODO: this does not check all capture groups
                if let Some(captures) = regex.captures(&test) {
                    if captures.iter().filter_map(|x| x).any(|capture| {
                        self.choices
                            .keys()
                            .any(|key| capture.as_str().starts_with(key.literal().as_ref()))
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
                        .find(|option| option.literal().as_ref().starts_with(capture.as_str()))
                    {
                        Some(format!(
                            "{}{}{}",
                            &arg[..capture.start()],
                            choice.literal().as_ref(),
                            &arg[capture.end()..]
                        ))
                    } else {
                        None
                    }
                }));
            }
        } else {
            results.extend(
                self.choices
                    .iter()
                    .filter(|(_, desc)| desc.check(counters))
                    .flat_map(|(choice, _)| choice.resolve(arg)),
            )
        }
    }
}

impl Sentinel {
    pub const fn new(
        counter: u8,
        check: Option<(Ordering, u8)>,
        assignment: Option<(Operator, u8)>,
    ) -> Self {
        Self { counter, check, assignment }
    }

    pub fn check(&self, counter: u8) -> bool {
        self.check.map_or(true, |(test, value)| counter.cmp(&value) == test)
    }

    pub fn check_and_update(&self, counter: &mut u8) -> bool {
        if self.check(*counter) {
            if let Some((op, value)) = &self.assignment {
                match op {
                    Operator::Dec => *counter -= value,
                    Operator::Inc => *counter += value,
                    Operator::Set => *counter = *value,
                }
            }
            true
        } else {
            false
        }
    }
}

impl Searcher {
    pub fn new(num_counters: u8, step: u8) -> Self {
        Self { counters: vec![0; num_counters as usize], step, completion: None }
    }

    pub fn step(&mut self) { self.step += 1; }
}

impl<'a, T: ToString + std::fmt::Debug + AsRef<str> + Ord> VMSearcher<'a, T> {
    pub fn new(def: &'a Definition<T>, args: &'a AutocompRequest) -> Self {
        Self { def, stack: vec![Searcher::new(def.num_counters, 0)], args }
    }

    pub fn choices(mut self) -> Result<Vec<String>, Error> {
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
                self.stack.retain_mut(|searcher| match def.steps.get(searcher.step as usize) {
                    Some(Step::Check(ref arg_def)) => {
                        searcher.step();
                        if let Some(regex) = &arg_def.regex {
                            if let Some(captures) = regex.captures(arg) {
                                for capture in captures.iter().filter_map(|x| x) {
                                    for (choice, desc) in &arg_def.choices {
                                        if capture.as_str().starts_with(choice.literal().as_ref()) {
                                            if let Some(sentinel) = &desc.sentinel {
                                                sentinel.check_and_update(
                                                    &mut searcher.counters
                                                        [sentinel.counter as usize],
                                                );
                                            }
                                        }
                                    }
                                }
                                true
                            } else {
                                false
                            }
                        } else {
                            arg_def.choices.iter().any(|(choice, desc)| {
                                arg.starts_with(choice.literal().as_ref())
                                    && desc.sentinel.as_ref().map_or(true, |sentinel| {
                                        sentinel.check_and_update(
                                            &mut searcher.counters[sentinel.counter as usize],
                                        )
                                    })
                            })
                        }
                    }
                    None | Some(Step::Match) => false,
                    _ => unreachable!(),
                });
            }
        }

        let VMSearcher { mut stack, args, def } = self;
        let arg = &args.argv()[args.word];

        let mut results = Vec::with_capacity(20);
        stack.sort_unstable_by_key(|searcher| searcher.completion);
        stack.dedup_by_key(|searcher| searcher.completion);
        for searcher in stack {
            if let Some(completion) = searcher.completion {
                if let Step::Check(check) = &def.steps[completion as usize] {
                    check.resolve(&mut results, arg, &searcher.counters);
                } else {
                    unreachable!()
                }
            }
        }
        Ok(results)
    }
}
