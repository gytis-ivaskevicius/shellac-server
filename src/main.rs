#![warn(clippy::all, clippy::pedantic, clippy::nursery, clippy::cargo)]

mod completion;
mod parser;
mod types;

// Codec definition
#[allow(dead_code)]
mod shellac_capnp {
    include!(concat!(env!("OUT_DIR"), "/shellac_capnp.rs"));
}

use self::{
    completion::{Definition, VMSearcher},
    types::Error,
};

use std::{
    convert::{TryFrom, TryInto},
    fs::{self, File},
    io::{self, BufRead, BufReader, BufWriter, Write},
    os::unix::net::UnixListener,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
    thread,
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

    /// Custom location for the socket
    #[structopt(short, long, parse(from_os_str))]
    socket: Option<PathBuf>,
}

fn get_comp_file(argv0: &str) -> io::Result<PathBuf> {
    // WONTFIX: Does not work on windows
    let path = &Path::new(argv0);
    let path = Path::new("completion").join(&path.file_name().unwrap());
    // TODO: check if this is not a file
    Ok(path.with_extension("shellac"))
}

fn handle_client<R: BufRead, W: Write>(
    mut reader: R,
    writer: W,
    cache: &Mutex<LruCache<String, Definition<String>>>,
) -> Result<(), Error> {
    use std::time::Instant;

    let mut writer = BufWriter::new(writer);
    // let mut codec = ArgvCodec::new(BufReader::new(reader));
    // while let Some(request) = codec.decode()? {
    loop {
        if reader.fill_buf().unwrap().is_empty() {
            break;
        }

        let request = match capnp::serialize::read_message(
            &mut reader,
            capnp::message::ReaderOptions::default(),
        ) {
            Ok(r) => r,
            Err(ref err) if err.kind == capnp::ErrorKind::Disconnected => break,
            Err(err) => {
                eprintln!("Could not decode request: {}", err);
                std::process::exit(1);
            }
        };
        let request = request.get_root::<shellac_capnp::request::Reader>().unwrap();
        if request.get_word() as u32 > request.get_argv().unwrap().len() {
            eprintln!(
                "Can't autocomplete word {} in argv of length {}",
                request.get_word(),
                request.get_argv().unwrap().len()
            );
            continue;
        }
        let start = Instant::now();
        let name = request.get_argv().unwrap().get(0).unwrap();
        let mut lock = match cache.lock() {
            Ok(v) => v,
            Err(_err) => {
                eprintln!("Mutex poisoned, could not access cache");
                std::process::exit(1);
            }
        };

        // Name should not need to be converted to a String, but the LRU cache requires it for some
        // reason.
        let choices = if let Some(def) = lock.get(&name.to_string()) {
            let start = Instant::now();
            let choices = VMSearcher::new(def).choices(&request)?;
            eprintln!("Time elapsed: {:?}", start.elapsed());
            std::mem::drop(lock);
            choices
        } else {
            let path = get_comp_file(name)?;

            let mut file = File::open(path)?;
            let def = match serde_yaml::from_reader::<_, parser::Definition>(&mut file) {
                Ok(d) => d,
                Err(err) => {
                    eprintln!("Failed to parse definition file for '{}': {}", name, err);
                    continue;
                }
            };
            let def = Definition::try_from(def)?;
            let start = Instant::now();
            let choices = VMSearcher::new(&def).choices(&request)?;
            eprintln!("Time elapsed: {:?}", start.elapsed());
            lock.put(name.to_string(), def);
            std::mem::drop(lock);
            choices
        };
        let mut message = capnp::message::Builder::new_default();
        let reply = message.init_root::<shellac_capnp::response::Builder>();

        let mut reply_choices =
            reply.init_choices(choices.len().try_into().expect("Too many output choices"));
        for (i, choice) in choices.iter().enumerate() {
            let mut reply_choice = reply_choices.reborrow().get(i as u32);
            reply_choice.set_arg(choice);
            reply_choice.set_description("");
        }

        capnp::serialize::write_message(&mut writer, &message).unwrap();
        eprintln!("Time elapsed: {:?}", start.elapsed());
    }
    Ok(())
}

fn main() {
    let opts = Opts::from_args();
    let cache = Arc::new(Mutex::new(LruCache::new(20)));

    if let Some(path) = opts.socket {
        let listener = UnixListener::bind(&path).unwrap_or_else(|_| {
            fs::remove_file(&path).unwrap();
            UnixListener::bind(&path).unwrap()
        });

        for stream in listener.incoming() {
            match stream {
                Ok(stream) => {
                    let cache = cache.clone();
                    thread::spawn(move || {
                        if let Err(err) = handle_client(BufReader::new(&stream), &stream, &cache) {
                            eprintln!("Could not execute request: {}", err)
                        }
                    });
                }
                Err(err) => {
                    eprintln!("Could not accept socket request {}", err);
                    std::process::exit(1);
                }
            }
        }
    } else if let Err(err) = handle_client(io::stdin().lock(), io::stdout().lock(), &cache) {
        eprintln!("Could not execute request: {}", err);
        std::process::exit(1);
    }
}
