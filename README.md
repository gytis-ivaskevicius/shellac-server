See https://github.com/oilshell/oil/wiki/Shellac-Protocol-Proposal-V2 and https://www.redox-os.org/news/rsoc-ion-ux-2/ for information about this repo

# Goals
The end result aims toward being a binary that
 - Replies to requests with minimal latency. Ideally, less than 10ms for static completion so it's not even visible for users
 - Is able to support as-you-type completion. That means caching should be used a lot to avoid duplicating work
 - Handle multiple (up to ~20) concurrent clients while keeping latency low for each one of them
 - Support definitions of parameters
 - Support invoking various external commands for dynamic completion

# Spec

See [the original](./CLAP.orig) and [adapted](./CLAP.proposed) proposals for formal specs.

Informal demo shellac definition files can be found in the [completion folder](./completion).

# Integration with shells

Communication is handled via cap'n proto using the definition found in [shellac.capnp](./shellac.capnp). Shell can choose to start a binary per request (simple but slow), or the recommended setup of using a socket for shared, extended communications.
This is a preliminary version, and as such the API is not yet finalised. Focus is currently more on stabilizing the definition file format rather than the shell-facing protocol, so change is to be expected.

# Contributing
Reach out at https://oilshell.zulipchat.com/#narrow/stream/146045-shell-autocompletion for discussion and feel free to open issues and MR. Any help is really appreciated.

# WARNING: THIS IS A POC
