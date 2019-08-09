pub mod completion;
mod errors;
pub mod parser;

// Codec definition
#[allow(dead_code)]
pub mod shellac_capnp {
    include!(concat!(env!("OUT_DIR"), "/shellac_capnp.rs"));
}

pub use shellac_capnp as codec;

pub use errors::Error;
