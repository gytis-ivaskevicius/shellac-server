pub mod completion;
mod errors;
pub mod parser;

// Codec definition
#[allow(dead_code)]
mod shellac_capnp {
    include!(concat!(env!("OUT_DIR"), "/shellac_capnp.rs"));
}

pub mod codec;

pub use errors::Error;
