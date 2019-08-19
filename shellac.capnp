@0xf0505ed189507b0d;

using Cxx = import "/capnp/c++.capnp";
$Cxx.namespace("shellac");

struct Request {
  argv @0 :List(Text);
  word @1 :UInt16;
}

struct Response {
  choices @0 :List(Suggestion);
}

struct Command {
  args @0 :List(Text);
  prefix @1 :Text;
}

struct Suggestion {
  arg :union {
    literal @0 :Text;
    command @2 :Command;
  }
  description @1 :Text;
}
