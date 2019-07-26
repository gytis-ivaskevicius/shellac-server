use docopt::parse::Parser;

use std::fs::File;
use std::io::BufReader;
use std::io::Read;

use super::AutocompRequest;
use super::Error;

#[derive(Debug, Clone)]
pub struct DocOptSearch<'a> {
    request: &'a AutocompRequest,
    docopt: &'a str,
}

#[derive(Debug, Clone)]
pub enum SearchResult {
    Literals(Vec<String>),
    Lookup(String),
}

impl<'a> DocOptSearch<'a> {
    fn new<T: AsRef<str> + 'a>(request: &'a AutocompRequest, docopt: &'a T) -> Self {
        Self {
            request,
            docopt: docopt.as_ref(),
        }
    }

    fn choices(self) -> Result<SearchResult, Error> {
        let docopt = Parser::new(self.docopt).map_err(Error::DocOpt)?;
        println!("Docopt: {:?}", docopt);
        let options_first = true;
        let mut do_flags = true;

        for (i, arg) in self.request.argv().iter().enumerate().skip(1) {
            let is_tabbed = self.request.word == i;
            if do_flags && arg.starts_with("--") && is_tabbed {
                let mut parts = arg.splitn(2, '=');
                let _option = &parts.next().unwrap()[2..];
                if let Some(_value) = parts.next() {
                    // Complete argument
                } else {
                    // Complete long flag
                }
            } else if arg.starts_with("-") && is_tabbed && do_flags {
                // List short options
            } else if arg == "--" {
                do_flags = false;
            } else {
                // Move parser
                if options_first {
                    do_flags = false;
                }
                // Check for positionals
            }
        }
        Ok(SearchResult::Literals(Vec::new()))
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

        let mut parts = content.splitn(2, "\n+++\n");
        // TODO: Add errors
        let docopt = parts.next().expect("Not a valid completion file");
        match DocOptSearch::new(&self.request, &docopt.trim()).choices()? {
            SearchResult::Literals(literals) => return Ok(super::Result { choices: literals }),
            SearchResult::Lookup(_name) => unimplemented!(),
        }
    }
}
