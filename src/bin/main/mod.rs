#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]
mod codec;
mod completion;
mod errors;
mod parser;

use self::completion::{Definition, VMSearcher};
use errors::Error;
use shellac;

// Codec definition
#[allow(dead_code)]
mod shellac_capnp {
    include!(concat!(env!("OUT_DIR"), "/shellac_capnp.rs"));
}
use shellac_capnp::{request::Reader as RequestReader, response::Builder as ResponseBuilder};

use std::{
    convert::TryFrom,
    fs::{self, File},
    io::{self, BufRead, BufReader, BufWriter, ErrorKind, Write},
    os::unix::net::UnixListener,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
    thread,
    time::Instant,
};

use lru::LruCache;
use structopt::StructOpt;

/// A Shell Agnostic Completion server
#[derive(StructOpt, Debug)]
#[structopt(name = "shellac-server")]
struct Opts {
    /// Activate debug mode
    #[structopt(short, long)]
    debug: bool,

    /// Verbose mode (-v, -vv, -vvv, etc.)
    #[structopt(short, long, parse(from_occurrences))]
    verbose: u8,

    /// Fallback completion (typically bash, zsh, fish)
    #[structopt(long)]
    fallback: Vec<String>,

    /// Completion for the shell's builtins
    #[structopt(long, parse(from_os_str))]
    shellcomp: Option<PathBuf>,

    /// Use a socket rather than the default stdin/out communication
    /// This is the way shells should communicate with shellac ideally
    #[structopt(short, long, parse(from_os_str))]
    socket: Option<PathBuf>,

    /// Completion for the shell's builtins
    #[structopt(short, long)]
    language: Option<String>,
}

/// Get the completion file given the first argument
fn get_comp_file(argv0: &str) -> io::Result<PathBuf> {
    // WONTFIX: Does not work on windows
    let path = &Path::new(argv0);
    let path = Path::new("completion").join(&path.file_name().unwrap());
    // TODO: check if this is not a file
    Ok(path.with_extension("shellac"))
}

/// Reply to requests until a client closes the read pipe
fn handle_client<R: BufRead, W: Write>(
    lang: &str,
    mut reader: R,
    writer: W,
    cache: &Mutex<LruCache<String, Definition<String>>>,
) -> Result<(), Error> {
    let mut writer = BufWriter::new(writer);
    // Check if another request was made
    while !reader.fill_buf()?.is_empty() {
        codec::read_request::<_, _, Error, _>(&mut reader, |_word, argv, request| {
            let start = Instant::now();
            let name = argv.get(0).map_err(shellac::Error::from)?;
            let mut lock = cache.lock()?;

            let search = |def| -> Result<_, Error> {
                let start = Instant::now();
                let choices = VMSearcher::new(def).choices(lang, request)?;
                eprintln!("Time elapsed: {:?}", start.elapsed());
                Ok(choices)
            };

            // Name should not need to be converted to a String, but the LRU cache requires it
            // for some reason.
            let choices = if let Some(def) = lock.get(&name.to_string()) {
                // If the definition already exists, use it and search the VM
                search(def)
            } else {
                // Else parse the definition
                let path = get_comp_file(name)?;

                let mut file = File::open(path)?;
                let def = match serde_yaml::from_reader::<_, parser::Definition>(&mut file) {
                    Ok(d) => d,
                    Err(err) => {
                        eprintln!("Failed to parse definition file for '{}': {}", name, err);
                        // TODO: return proper error
                        return Ok(());
                    }
                };
                let def = Definition::try_from(def)?;
                let choices = search(&def);
                // Save the definition in the cache for later use
                lock.put(name.to_string(), def);
                choices
            }?;
            // Drop the lock the earliest possible to avoid stalling
            std::mem::drop(lock);
            codec::write_reply(&mut writer, &choices)?;
            eprintln!("Time elapsed: {:?}", start.elapsed());
            Ok(())
        })?;
    }
    Ok(())
}

fn main() {
    let opts = Opts::from_args();
    let cache = Arc::new(Mutex::new(LruCache::new(20)));

    // Process on socket if one was specified
    if let Some(path) = opts.socket {
        let listener = UnixListener::bind(&path)
            .or_else(|_| {
                // If the socket can't be opened, retry after removing a leftover
                fs::remove_file(&path)?;
                UnixListener::bind(&path)
            })
            .unwrap();

        let language = Arc::new(opts.language);
        for stream in listener.incoming() {
            match stream {
                Ok(stream) => {
                    let language = language.clone();
                    let cache = cache.clone();
                    // Start a thread to handle the client without blocking the main thread
                    // As the expected number of client should be fairly low, there is no need for
                    // non-blocking IO, and simple thread should suffice
                    thread::spawn(move || {
                        if let Err(err) = handle_client(
                            language.as_ref().as_ref().map_or("en", String::as_str),
                            BufReader::new(&stream),
                            &stream,
                            &cache,
                        ) {
                            eprintln!("Could not execute request: {}", err)
                        }
                    });
                }
                Err(err) => {
                    eprintln!("Could not accept socket request: {}", err);
                    if err.kind() != ErrorKind::ConnectionAborted {
                        std::process::exit(1);
                    }
                }
            }
        }
    // Else use stdin/out
    } else if let Err(err) = handle_client(
        opts.language.as_ref().map_or("en", String::as_str),
        io::stdin().lock(),
        io::stdout().lock(),
        &cache,
    ) {
        eprintln!("Could not execute request: {}", err);
        std::process::exit(1);
    }
}
