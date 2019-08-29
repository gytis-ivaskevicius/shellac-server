use regex::Regex;
use retain_mut::RetainMut;

use std::{
    borrow::Cow,
    cmp::{Ord, Ordering, PartialOrd},
    collections::BTreeMap,
};

use super::{Error, RequestReader};
use shellac::{Suggestion, SuggestionType};

/// A step in matching the definition
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Step<T: Ord> {
    /// Check if one value of argv corresponds
    Check(Arg<T>),
    /// Jump to a step in the process
    Jump(u8),
    /// Create a new thread that spawns at the specified step
    Split(u8),
    /// This is a match
    Match,
}

/// Descriptions in various languages
pub type Descriptions = Vec<BTreeMap<String, String>>; // A HashMap is clearer, but a vec is
                                                       // faster
/// A non-owned version of Descriptions
type DescriptionsRef<'a> = &'a [BTreeMap<String, String>];

/// Parts to lookup (ex: <file>)
pub type Definitions = BTreeMap<String, String>;

/// The complete definition of a `ShellAC` specification
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Definition<T: Ord> {
    /// The steps to match the definition
    pub steps: Vec<Step<T>>,
    /// The number of counters a thread should hold
    pub num_counters: u8,
    /// The localized description reference
    pub descriptions: Descriptions,
    /// A description of `<...>`'s meaning
    pub definitions: BTreeMap<String, String>,
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

/// The definition of a given choice
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Choice {
    /// An optional description index
    description: Option<usize>,
    /// An optional description of its sentinel
    sentinel: Option<Sentinel>,
}

/// A guard for matching
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Sentinel {
    /// Which counter to target
    counter: u8,
    /// What should the counter match
    check: Option<(Ordering, u8)>,
    /// How to update the counter afterward
    assignment: Option<Operator>,
}

/// Modifications on a counter
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    /// Increment it
    Inc(u8),
    /// Decrement it
    Dec(u8),
    /// Set to a specific value
    Set(u8),
}

/// An argument. Literals are matched exactly, and a sequence of lookups followed by literals can
/// be used afterward
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Argument<T> {
    literal:   T,
    reference: Option<Vec<(T, T)>>,
}

/// A VM style searcher for the definition searcher
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VMSearcher<'a, 'b, T: Ord> {
    /// The search definition
    def: &'a Definition<T>,
    /// A set of threads looking up a particular path
    stack: Vec<Thread<'b>>,
}

/// One thread that searches for a match
#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub struct Thread<'a> {
    /// A set of one-byte counters. Copy on Write to avoid needless allocations
    counters: Cow<'a, [u8]>,
    /// Which step is the thread currently at in the definition's list of steps
    step: u8,
    /// The potential match for the definition. For example with `git ch<tab>` and definition
    /// `git => checkout`, the steps would be `Match("git"), Match("checkout")`, the thread would
    /// have Some(1) as it's completion, as it's Match("checkout") that coresponds to the argument
    /// being autocompleted in the sequence
    completion: Option<u8>,
}

impl<T> Argument<T> {
    /// Get the literal prefix to match
    pub const fn literal(&self) -> &T { &self.literal }

    /// Create a new argument
    pub const fn new(literal: T, reference: Option<Vec<(T, T)>>) -> Self {
        Self { literal, reference }
    }
}

impl<T: AsRef<str>> Argument<T> {
    /// Take an argument and generate a possible suggestion
    pub fn resolve<'a, O: From<&'a str>>(
        &'a self,
        start: &'a str,
        defs: &'a Definitions,
    ) -> Option<SuggestionType<O>> {
        let literal = self.literal.as_ref();
        match &self.reference {
            // If only a literal is used, return the choice minus the end of the predicate
            None if literal.starts_with(start) => {
                Some(SuggestionType::Literal(O::from(&self.literal.as_ref()[start.len()..])))
            }
            // Else if the typed part starts with the literal
            Some(reference) if start.starts_with(literal) => {
                let mut start = &start[literal.len()..];
                for (reference, postfix) in reference {
                    // If the prefix is found, trim it
                    if let Some(pos) = start.find(postfix.as_ref()) {
                        start = &start[pos..];
                    // Else return the a potential autocompletion match
                    } else {
                        let command = defs.get(reference.as_ref())?;
                        // TODO: allow single quotes and other methods that exec
                        if !command.starts_with("exec \"") || !command.ends_with('"') {
                            return None;
                        }
                        return Some(SuggestionType::Command {
                            command: command.as_str()[6..command.len() - 1]
                                .split("\" \"")
                                .map(O::from)
                                .collect(),
                            prefix:  O::from(start),
                        });
                    }
                }
                None
            }
            // Else if literal starts with the typed text, autocomplete the literal
            Some(_) if literal.starts_with(start) => {
                Some(SuggestionType::Literal(O::from(&self.literal.as_ref()[start.len()..])))
            }
            // Else, no match
            _ => None,
        }
    }
}

impl Choice {
    pub const fn new(description: Option<usize>, sentinel: Option<Sentinel>) -> Self {
        Self { description, sentinel }
    }

    /// Does the sentinel match?
    pub fn check(&self, counters: &[u8]) -> bool {
        self.sentinel
            .as_ref()
            .map_or(true, |sentinel| sentinel.check(counters[sentinel.counter as usize]))
    }

    /// Get the description for a choice. Defaults to en if the language is not found
    pub fn description(&self, lang: &str, descs: DescriptionsRef<'_>) -> String {
        self.description
            .as_ref()
            .map(|desc| descs.get(*desc).expect("Invalid index for description"))
            .map_or_else(String::new, |descs| {
                descs.get(lang).or_else(|| descs.get("en")).unwrap().clone()
            })
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

/// Find the number of chars at the end of `start` that overlap with the start of `end`
///
/// Ex: overlap("abcdef", "defghij") = 3
fn overlap(start: &str, end: &str) -> usize {
    for t in 1..=end.len() {
        if start.ends_with(&end[..t]) {
            return t;
        }
    }
    0
}

/// Get all the values that maximises a given arg. Returns None if no value were provided
fn argmaxes<T, S: Ord, F: Fn(&T) -> S, I: Iterator<Item = T>>(
    mut options: I,
    f: F,
) -> Option<(Vec<T>, S)> {
    if let Some(option) = options.next() {
        let mut out = Vec::with_capacity(options.size_hint().0);
        let mut max = f(&option);
        out.push(option);

        for option in options {
            let value = f(&option);
            match value.cmp(&max) {
                Ordering::Equal => out.push(option),
                Ordering::Greater => {
                    max = value;
                    out.clear();
                    out.push(option);
                }
                Ordering::Less => (),
            }
        }
        Some((out, max))
    } else {
        None
    }
}

// Once we get an arg as a potential match, list all matches for it.
//
// Ex:
// -- => list long options
// --p => age=
// --page= => <file>
impl<T: Ord + AsRef<str>> Arg<T> {
    pub fn resolve<'a>(
        &'a self,
        lang: &str,
        results: &mut Vec<Suggestion<String>>,
        arg: &str,
        counters: &[u8],
        defs: &Definitions,
        descs: DescriptionsRef,
    ) {
        if let Some(regex) = &self.regex {
            // Get the option with the maximum overlap with the typed value. This only considers
            // the literal, so for example `--long-file` will have higher precedence over
            // `--long=<file>`
            let (temp, prefix) = argmaxes(
                self.choices.iter().filter(|(_, desc)| desc.check(counters)),
                |(option, _)| overlap(arg, option.literal().as_ref()),
            )
            .expect("No choices for description");

            // Reuse the same string to avoid allocation
            let len = arg.len();
            let mut test = arg.to_string();
            for (option, check) in temp {
                if option.literal().as_ref().len() == prefix {
                    // Only if there is a reference, else the same text will be autocompleted,
                    // which is probably not whished by the user
                    if option.reference.is_some() {
                        if let Some(suggestion) = option.resolve(&arg[arg.len() - prefix..], defs) {
                            results
                                .push(Suggestion::new(suggestion, check.description(lang, descs)));
                        }
                    }
                } else {
                    // Clear the text and append the option's literal without the matched overlap
                    test.truncate(len);
                    test.push_str(&option.literal().as_ref()[prefix..]);

                    // TODO: this does not check all capture groups. Check that only a single
                    // capture group more was matched?
                    if let Some(captures) = regex.captures(&test) {
                        // Test if a match is found for capture groups that were added
                        let captures_match = captures.iter().filter_map(|x| x).any(|capture| {
                            self.choices
                                .keys()
                                .any(|key| capture.as_str().starts_with(key.literal().as_ref()))
                        });
                        if captures_match {
                            results.push(Suggestion::new(
                                SuggestionType::Literal(option.literal().as_ref()[prefix..].into()),
                                check.description(lang, descs),
                            ));
                        }
                    }
                }
            }
        } else {
            // If no regex, simply propose all the options
            results.extend(self.choices.iter().filter(|(_, desc)| desc.check(counters)).filter_map(
                |(choice, check)| {
                    choice
                        .resolve(arg, defs)
                        .map(|c| Suggestion::new(c, check.description(lang, descs)))
                },
            ))
        }
    }
}

impl Sentinel {
    pub const fn new(
        counter: u8,
        check: Option<(Ordering, u8)>,
        assignment: Option<Operator>,
    ) -> Self {
        Self { counter, check, assignment }
    }

    pub fn check(&self, counter: u8) -> bool {
        self.check.map_or(true, |(test, value)| counter.cmp(&value) == test)
    }

    /// Check if the counter matches and if so update the counter
    pub fn check_and_update(&self, counter: &mut u8) -> bool {
        if self.check(*counter) {
            if let Some(op) = &self.assignment {
                match op {
                    Operator::Dec(value) => *counter -= value,
                    Operator::Inc(value) => *counter += value,
                    Operator::Set(value) => *counter = *value,
                }
            }
            true
        } else {
            false
        }
    }
}

impl Thread<'_> {
    pub fn new(num_counters: u8, step: u8) -> Self {
        Self { counters: Cow::Owned(vec![0; num_counters as usize]), step, completion: None }
    }

    pub fn step(&mut self) { self.step += 1; }
}

impl<'a, T: Ord> VMSearcher<'a, '_, T> {
    pub fn new(def: &'a Definition<T>) -> Self {
        Self { def, stack: vec![Thread::new(def.num_counters, 0)] }
    }
}

impl<'a, T: AsRef<str> + Ord> VMSearcher<'a, '_, T> {
    /// Search and suggest all the valid option completion
    pub fn choices(
        mut self,
        lang: &str,
        args: &RequestReader,
    ) -> Result<Vec<Suggestion<String>>, Error> {
        let argv = args.get_argv().unwrap();
        let word = args.get_word();
        // TODO: This will take down the server if a request is not well formed
        for (i, arg) in argv.iter().map(Result::unwrap).enumerate().skip(1) {
            // Advance to the next argument, processing jumps and splits. This avoid keeping a
            // reference to the currently parsed arg in the threads. TODO: is this needed?
            while self.stack.iter().any(|searcher| {
                if let Step::Check(..) | Step::Match = self.def.steps[searcher.step as usize] {
                    false
                } else {
                    true
                }
            }) {
                // Iterate explicitely to be able to push to the end of the vector. TODO: is there
                // a more "rusty" way of doing this?
                let stack_len = self.stack.len();
                for j in 0..stack_len {
                    let searcher = &mut self.stack[j];

                    match self.def.steps[searcher.step as usize] {
                        Step::Jump(i) => searcher.step = i,
                        // Create a new thread at a specific step
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

            // If the autocompleted word is this one
            if i == word as usize {
                // Only borrow the definition
                let def = self.def;

                self.stack.retain_mut(move |searcher| {
                    // If the step is outside the number of steps, we're trying to continue
                    // matching after a match, so delete the thread
                    if searcher.step as usize >= def.steps.len() {
                        return false;
                    }

                    match &def.steps[searcher.step as usize] {
                        Step::Check(..) => {
                            // The current arg is the one that will need to be expanded at the end
                            searcher.completion = Some(searcher.step);
                            searcher.step();
                            true
                        }
                        // If already on a match, delete the thread
                        Step::Match => false,
                        _ => unreachable!(),
                    }
                })
            } else {
                let def = self.def;
                self.stack.retain_mut(|searcher| match def.steps.get(searcher.step as usize) {
                    Some(Step::Check(ref arg_def)) => {
                        searcher.step();

                        if let Some(regex) = &arg_def.regex {
                            if let Some(captures) = regex.captures(arg) {
                                // Check that (ideally all) capture group are valids
                                // TODO: this only works for one capture group of the same kind
                                for capture in captures.iter().filter_map(|x| x) {
                                    for (choice, desc) in &arg_def.choices {
                                        if capture.as_str().starts_with(choice.literal().as_ref()) {
                                            // If we found one, update the counter
                                            if let Some(sentinel) = &desc.sentinel {
                                                sentinel.check_and_update(
                                                    &mut searcher.counters.to_mut()
                                                        [sentinel.counter as usize],
                                                );
                                            }
                                        }
                                    }
                                }
                                true
                            } else {
                                // If the regex did not match, delete the thread
                                false
                            }
                        } else {
                            // In case of a literal, assert that a choice meets the argument
                            arg_def.choices.iter().any(|(choice, desc)| {
                                arg.starts_with(choice.literal().as_ref())
                                    && desc.sentinel.as_ref().map_or(true, |sentinel| {
                                        // Use short-cuting to update the sentinel automatically if
                                        // there is a match but not otherwise
                                        sentinel.check_and_update(
                                            &mut searcher.counters.to_mut()
                                                [sentinel.counter as usize],
                                        )
                                    })
                            })
                        }
                    }
                    // If we're after the end, or at a match, then delete the thread
                    None | Some(Step::Match) => false,
                    _ => unreachable!(),
                });
            }
        }

        let arg = &argv.get(u32::from(word)).unwrap();

        let mut results = Vec::with_capacity(20);

        // Remove duplicates
        self.stack.sort_unstable_by_key(|searcher| searcher.completion);
        self.stack.dedup_by_key(|searcher| searcher.completion);

        // For all the threads
        for searcher in self.stack {
            // If no completion was found, something is astray
            let completion =
                searcher.completion.expect("Did not pass by the word to autocomplete?");
            if let Step::Check(check) = &self.def.steps[completion as usize] {
                // Add the possible completions to the results
                check.resolve(
                    lang,
                    &mut results,
                    arg,
                    &searcher.counters,
                    &self.def.definitions,
                    &self.def.descriptions,
                );
            } else {
                unreachable!()
            }
        }
        Ok(results)
    }
}
