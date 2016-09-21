# Epimorphism

Epimorphism is a psychedelic virtualization of video feedback.  It is written in `pure_script` and compiles to javascript.

The project is live on the web here http://www.epimorphism.com

## Setup

- get setup with purescript http://www.purescript.org/learn/getting-started/
-  You need to use purescript 0.8.5, so in the above instructions
-  so,  `npm install -g purescript@0.8.5` and `npm install -g pulp@8.2.1`
-  BUT!  This won't install with node 6, so you need node 5 or 4.
- install purescript dependencies - `bower i`
- install fswatch - easily done with homebrew `brew install fswatch`
- install http-server - `npm install http-server -g`
- start the build system - `scripts/watch.sh`
- start the development server - `scripts/server.sh`
- the project should be visible on http://localhost:8000


## Useful Commands
 - `~` - development panel
 - `|` - show fps
