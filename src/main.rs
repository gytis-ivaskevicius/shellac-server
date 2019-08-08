use shellac_server::{
    completion::{Definition, VMSearcher},
    parser, AutocompRequest,
};

use std::{
    convert::TryFrom,
    fs::{self, File},
    io::{self, BufReader, BufWriter, Read, Write},
    os::unix::net::UnixListener,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
    thread,
};

use lru::LruCache;
use serde_json::Deserializer;
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

fn handle_client<R: Read, W: Write>(
    reader: R,
    writer: W,
    cache: &Mutex<LruCache<String, Definition<String>>>,
) -> Result<(), shellac_server::Error> {
    use std::time::Instant;

    let mut writer = BufWriter::new(writer);
    // let mut codec = ArgvCodec::new(BufReader::new(reader));
    // while let Some(request) = codec.decode()? {
    for request in Deserializer::from_reader(BufReader::new(reader)).into_iter::<AutocompRequest>()
    {
        let request = request.unwrap();
        let start = Instant::now();
        let name = &request.argv()[0];
        let mut lock = match cache.lock() {
            Ok(v) => v,
            Err(_err) => {
                eprintln!("Mutex poisoned, could not access cache");
                std::process::exit(1);
            }
        };
        let choices = match lock.get(name) {
            Some(def) => {
                let start = Instant::now();
                let choices = VMSearcher::new(def, &request).choices();
                eprintln!("Time elapsed: {:?}", start.elapsed());
                std::mem::drop(lock);
                choices
            }
            None => {
                let path = get_comp_file(name)?;

                let mut file = File::open(path)?;
                let mut content = String::with_capacity(1024);
                file.read_to_string(&mut content)?;

                let def = serde_yaml::from_str::<parser::Definition>(&content).unwrap();
                let def = Definition::try_from(def).unwrap();
                let start = Instant::now();
                let choices = VMSearcher::new(&def, &request).choices();
                eprintln!("Time elapsed: {:?}", start.elapsed());
                lock.put(name.to_string(), def);
                std::mem::drop(lock);
                choices
            }
        }
        .unwrap();
        serde_json::to_writer(&mut writer, &choices).unwrap();
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
                    thread::spawn(move || match handle_client(&stream, &stream, &cache) {
                        Ok(()) => eprintln!("Socket closed"),
                        Err(err) => eprintln!("Could not execute request: {}", err),
                    });
                }
                Err(err) => {
                    eprintln!("Could not accept socket request {}", err);
                    break;
                }
            }
        }
    } else {
        if let Err(err) = handle_client(io::stdin().lock(), io::stdout().lock(), &cache) {
            eprintln!("Could not execute request: {}", err);
        }
    }
}
