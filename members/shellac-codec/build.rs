fn main() { capnpc::CompilerCommand::new().file("shellac.capnp").run().expect("compiling schema"); }
