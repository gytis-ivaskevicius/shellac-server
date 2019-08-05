use regex::Regex;
use retain_mut::RetainMut;

use std::cmp::Ordering;
use std::collections::BTreeMap;
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
    pub steps: Vec<Step>,
    pub num_counters: u8,
    pub descriptions: Vec<BTreeMap<String, String>>, // A HashMap is clearer, but a vec is faster
}

#[derive(Debug, Clone, Default)]
pub struct Arg {
    regex: Option<Regex>,
    // TODO: Is this the best datastructure? Can we access variables directly? If not, maybe a
    // simple vec with a binary search would be better
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
    description: Option<usize>,
    sentinel: Option<Sentinel>,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Sentinel {
    counter: u8,
    check: Option<(Ordering, u8)>,
    assignment: Option<(Operator, u8)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    Inc,
    Dec,
    Set,
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

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
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

impl Choice {
    pub fn check(&self, counters: &[u8]) -> bool {
        self.sentinel.as_ref().map_or(true, |sentinel| {
            sentinel.check(counters[sentinel.counter as usize])
        })
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
    pub const fn new(regex: Option<Regex>, choices: BTreeMap<ChoiceType, Choice>) -> Self {
        Self { regex, choices }
    }

    pub fn resolve(&self, results: &mut Vec<String>, arg: &str, counters: &[u8]) {
        if let Some(regex) = &self.regex {
            let len = arg.len();
            let mut test = arg.to_string();

            for (option, _) in self.choices.iter().filter(|(_, desc)| desc.check(counters)) {
                test.truncate(len);
                test.push_str(option.as_str());

                // TODO: this does not check all capture groups
                if let Some(captures) = regex.captures(&test) {
                    if captures.iter().filter_map(|x| x).any(|capture| {
                        self.choices
                            .keys()
                            .any(|key| capture.as_str().starts_with(key.as_str()))
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
    pub fn check(&self, counter: u8) -> bool {
        self.check
            .map_or(true, |(test, value)| counter.cmp(&value) == test)
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
    pub fn new(def: &'a Definition, args: &'a AutocompRequest) -> Self {
        Self {
            def,
            stack: vec![Searcher::new(def.num_counters, 0)],
            args,
        }
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
                self.stack
                    .retain_mut(|searcher| match def.steps.get(searcher.step as usize) {
                        Some(Step::Check(ref arg_def)) => {
                            searcher.step();
                            if let Some(regex) = &arg_def.regex {
                                if let Some(captures) = regex.captures(arg) {
                                    for capture in captures.iter().filter_map(|x| x) {
                                        for (choice, desc) in &arg_def.choices {
                                            if capture.as_str().starts_with(choice.as_str()) {
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
                                    arg.starts_with(choice.as_str())
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

        let VMSearcher {
            mut stack,
            args,
            def,
        } = self;
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

impl Definition {
    pub fn new<T: AsRef<str>>(_def: &T) -> Result<Self, regex::Error> {
        Ok(Self {
            num_counters: 1,
            descriptions: Vec::new(),
            steps: vec![
                Step::Split(11),
                Step::Split(4),
                Step::Check(Arg {
                    regex: Some(Regex::new(r"^-([a-zA-Z])+$|^--([a-zA-Z_\-=]{2,})$")?),
                    choices: vec![
                        ("version".parse().unwrap(), Choice::default()),
                        ("help".parse().unwrap(), Choice::default()),
                        ("exec-path".parse().unwrap(), Choice::default()),
                        ("exec-path=<file>".parse().unwrap(), Choice::default()),
                        ("html-path".parse().unwrap(), Choice::default()),
                        ("man-path".parse().unwrap(), Choice::default()),
                        ("info-path".parse().unwrap(), Choice::default()),
                        (
                            "p".parse().unwrap(),
                            Choice {
                                description: None,
                                sentinel: Some(Sentinel {
                                    counter: 0,
                                    check: Some((Ordering::Equal, 0)),
                                    assignment: Some((Operator::Set, 1)),
                                }),
                            },
                        ),
                        ("paginate".parse().unwrap(), Choice::default()),
                        ("P".parse().unwrap(), Choice::default()),
                        ("no-pager".parse().unwrap(), Choice::default()),
                        ("no-replace-objects".parse().unwrap(), Choice::default()),
                        ("bare".parse().unwrap(), Choice::default()),
                        ("git-dir=<file>".parse().unwrap(), Choice::default()),
                        ("work-tree=<file>".parse().unwrap(), Choice::default()),
                        ("namespace=<file>".parse().unwrap(), Choice::default()),
                        ("super-prefix=<file>".parse().unwrap(), Choice::default()),
                    ]
                    .into_iter()
                    .collect(),
                }),
                Step::Jump(0),
                Step::Split(8),
                Step::Check(Arg {
                    regex: None,
                    choices: vec![("-C".parse().unwrap(), Choice::default())]
                        .into_iter()
                        .collect(),
                }),
                Step::Check(Arg {
                    regex: None,
                    choices: vec![("<file>".parse().unwrap(), Choice::default())]
                        .into_iter()
                        .collect(),
                }),
                Step::Jump(0),
                Step::Check(Arg {
                    regex: None,
                    choices: vec![("-c".parse().unwrap(), Choice::default())]
                        .into_iter()
                        .collect(),
                }),
                Step::Check(Arg {
                    regex: None,
                    choices: vec![("<name>=<file>".parse().unwrap(), Choice::default())]
                        .into_iter()
                        .collect(),
                }),
                Step::Split(1),
                Step::Check(Arg {
                    regex: None,
                    choices: vec![
                        ("add".parse().unwrap(), Choice::default()),
                        ("am".parse().unwrap(), Choice::default()),
                        ("archive".parse().unwrap(), Choice::default()),
                        ("bisect".parse().unwrap(), Choice::default()),
                        ("branch".parse().unwrap(), Choice::default()),
                        ("bundle".parse().unwrap(), Choice::default()),
                        ("checkout".parse().unwrap(), Choice::default()),
                        ("cherry-pick".parse().unwrap(), Choice::default()),
                        ("citool".parse().unwrap(), Choice::default()),
                        ("clean".parse().unwrap(), Choice::default()),
                        ("clone".parse().unwrap(), Choice::default()),
                        ("commit".parse().unwrap(), Choice::default()),
                        ("describe".parse().unwrap(), Choice::default()),
                        ("diff".parse().unwrap(), Choice::default()),
                        ("fetch".parse().unwrap(), Choice::default()),
                        ("format-patch".parse().unwrap(), Choice::default()),
                        ("gc".parse().unwrap(), Choice::default()),
                        ("grep".parse().unwrap(), Choice::default()),
                        ("gui".parse().unwrap(), Choice::default()),
                        ("init".parse().unwrap(), Choice::default()),
                        ("log".parse().unwrap(), Choice::default()),
                        ("merge".parse().unwrap(), Choice::default()),
                        ("mv".parse().unwrap(), Choice::default()),
                        ("notes".parse().unwrap(), Choice::default()),
                        ("pull".parse().unwrap(), Choice::default()),
                        ("push".parse().unwrap(), Choice::default()),
                        ("range-diff".parse().unwrap(), Choice::default()),
                        ("rebase".parse().unwrap(), Choice::default()),
                        ("reset".parse().unwrap(), Choice::default()),
                        ("revert".parse().unwrap(), Choice::default()),
                        ("rm".parse().unwrap(), Choice::default()),
                        ("shortlog".parse().unwrap(), Choice::default()),
                        ("show".parse().unwrap(), Choice::default()),
                        ("stash".parse().unwrap(), Choice::default()),
                        ("status".parse().unwrap(), Choice::default()),
                        ("submodule".parse().unwrap(), Choice::default()),
                        ("tag".parse().unwrap(), Choice::default()),
                        ("worktree".parse().unwrap(), Choice::default()),
                        // Ancillary commands
                        ("config".parse().unwrap(), Choice::default()),
                        ("fast-export".parse().unwrap(), Choice::default()),
                        ("fast-import".parse().unwrap(), Choice::default()),
                        ("filter-branch".parse().unwrap(), Choice::default()),
                        ("mergetool".parse().unwrap(), Choice::default()),
                        ("pack-refs".parse().unwrap(), Choice::default()),
                        ("prune".parse().unwrap(), Choice::default()),
                        ("reflog".parse().unwrap(), Choice::default()),
                        ("remote".parse().unwrap(), Choice::default()),
                        ("repack".parse().unwrap(), Choice::default()),
                        ("replace".parse().unwrap(), Choice::default()),
                        // Interrogators
                        ("annotate".parse().unwrap(), Choice::default()),
                        ("blame".parse().unwrap(), Choice::default()),
                        ("count-objects".parse().unwrap(), Choice::default()),
                        ("difftool".parse().unwrap(), Choice::default()),
                        ("fsck".parse().unwrap(), Choice::default()),
                        ("help".parse().unwrap(), Choice::default()),
                        ("instaweb".parse().unwrap(), Choice::default()),
                        ("merge-tree".parse().unwrap(), Choice::default()),
                        ("rerere".parse().unwrap(), Choice::default()),
                        ("show-branch".parse().unwrap(), Choice::default()),
                        ("verify-commit".parse().unwrap(), Choice::default()),
                        ("verify-tag".parse().unwrap(), Choice::default()),
                        ("whatchanged".parse().unwrap(), Choice::default()),
                        // Interacting with others
                        ("archimport".parse().unwrap(), Choice::default()),
                        ("cvsexportcommit".parse().unwrap(), Choice::default()),
                        ("cvsimport".parse().unwrap(), Choice::default()),
                        ("cvsserver".parse().unwrap(), Choice::default()),
                        ("imap-send".parse().unwrap(), Choice::default()),
                        ("p4".parse().unwrap(), Choice::default()),
                        ("quiltimport".parse().unwrap(), Choice::default()),
                        ("request-pull".parse().unwrap(), Choice::default()),
                        ("send-email".parse().unwrap(), Choice::default()),
                        ("svn".parse().unwrap(), Choice::default()),
                    ]
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
