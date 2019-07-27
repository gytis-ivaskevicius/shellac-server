use shellac_server::{codec::ArgvCodec, completion};

use std::fs::{self, File};
use std::io::{self, BufWriter, Write};
use std::os::unix::net::{UnixListener, UnixStream};
use std::path::{Path, PathBuf};
use std::thread;

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

fn handle_client(stream: UnixStream) -> Result<(), shellac_server::Error> {
    use std::time::{Duration, Instant};
    let mut writer = BufWriter::new(&stream);
    let mut codec = ArgvCodec::new(&stream);
    while let Some(request) = codec.decode()? {
        let start = Instant::now();
        let path = get_comp_file(&request.argv()[0])?;
        let file = File::open(path)?;
        let completed = completion::complete(file, request)?;
        let duration = start.elapsed();
        serde_json::to_writer(&mut writer, &completed);
        writer.flush();
        println!("Time elapsed in expensive_function() is: {:?}", duration);
    }
    Ok(())
}

fn main() {
    let opts = Opts::from_args();

    let path = opts
        .socket
        .unwrap_or_else(|| PathBuf::from("/run/user/1000/shellac.sock"));

    let listener = UnixListener::bind(&path).unwrap_or_else(|_| {
        fs::remove_file(&path).unwrap();
        UnixListener::bind(&path).unwrap()
    });

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                thread::spawn(|| match handle_client(stream) {
                    Ok(()) => println!("Socket closed"),
                    Err(err) => println!("Could not execute request: {}", err),
                });
            }
            Err(err) => {
                println!("Could not accept socket request {}", err);
                break;
            }
        }
    }
}
