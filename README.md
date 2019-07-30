# WARNING: THIS IS A EARLY STAGE POC, IN NO WAY FIT FOR INTEGRATION
See https://github.com/oilshell/oil/wiki/Shellac-Protocol-Proposal-V2 and https://www.redox-os.org/news/rsoc-ion-ux-2/ for information about this repo

# Goals
The end result aims toward being a binary that
 - Replies to requests with minimal latency. Ideally, less than 10ms for static completion so it's not even visible for users
 - Is able to support as-you-type completion. That means caching should be used a lot to avoid duplicating work
 - Handle multiple (up to ~50) concurrent clients while keeping latency low for each one of them
 - Support definitions of parameters
 - Support invoking various commands

# Contributing
Reach AdminXVII at https://chat.redox-os.org/redox/channels/ion for things to do. This is still an fairly young POC, so any help is greatly appreciated.

