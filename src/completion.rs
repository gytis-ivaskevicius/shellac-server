use bytes::BytesMut;
use docopt::parse::Parser;
use tokio::fs::File;
use tokio::prelude::*;

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
    file: File,
    buf: BytesMut,
    request: AutocompRequest,
    state: State,
}

enum State {
    ReadingDocOpt,
    ReadingYaml,
    Done,
}

pub fn complete(file: File, request: AutocompRequest) -> Completer {
    Completer {
        request,
        buf: BytesMut::with_capacity(1024),
        file,
        state: State::ReadingDocOpt,
    }
}

impl Future for Completer {
    type Item = Vec<String>;
    type Error = Error;

    fn poll(&mut self) -> Result<Async<Self::Item>, Self::Error> {
        loop {
            match self.state {
                State::ReadingYaml => {
                    self.buf.reserve(1024);

                    let n = match { self.file.read_buf(&mut self.buf) } {
                        Ok(Async::NotReady) => return Ok(Async::NotReady),
                        Ok(Async::Ready(v)) => v,
                        Err(err) => return Err(err.into()),
                    };
                    if n == 0 {
                        self.state = State::Done;
                        return match std::str::from_utf8(&self.buf) {
                            Ok(v) => Ok(Async::Ready(vec![v.to_string()])),
                            Err(err) => Err(Error::FileNotUtf8(err)),
                        };
                    }
                }
                State::ReadingDocOpt => {
                    self.buf.reserve(1024);

                    let n = match { self.file.read_buf(&mut self.buf) } {
                        Ok(Async::NotReady) => return Ok(Async::NotReady),
                        Ok(Async::Ready(v)) => v,
                        Err(err) => return Err(err.into()),
                    };

                    let pos = self.buf.len() - n;
                    if let Some(mut position) = self.buf[pos.saturating_sub(5)..]
                        .windows(5)
                        .position(|window| window == b"\n+++\n")
                    {
                        position += pos;
                        let docopt = self.buf.split_to(position);
                        let docopt =
                            std::str::from_utf8(&docopt[..docopt.len().saturating_sub(5)])?.trim();
                        match DocOptSearch::new(&self.request, &docopt).choices()? {
                            SearchResult::Literals(literals) => {
                                self.state = State::Done;
                                return Ok(Async::Ready(literals));
                            }
                            SearchResult::Lookup(_name) => {
                                self.state = State::ReadingYaml;
                            }
                        }
                    }
                }
                State::Done => panic!("poll a Completer after it's done"),
            }
        }
    }
}
