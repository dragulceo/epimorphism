# Epimorphism

Epimorphism is a psychedelic virtualization of video feedback.  It is written in `pure_script` and compiles to javascript.  This is a port of the software from a previous version, and is currenty in development.

## Setup

- !!NOTE!!
-  Setting up this project isn't as simple as it should be.  You need to use purescript 0.8.5, so in the below instructions
-  you need `npm install -g purescript@0.8.5` and `npm install -g pulp@8.2.1`
-  BUT!  This won't install with node 6, so you need node 5 or 4.
- get setup with purescript http://www.purescript.org/learn/getting-started/
- install purescript dependencies - `bower i`
- install fswatch - easily done with homebrew `brew install fswatch`
- !! NOW !!
-  There is an issue with one of the dependencies, purescript-simple-dom.
-  The quick fix is to add these lines to bower_components/purescript-simple-dom/src/Data/DOM/Simple/Encode.js
-   exports.encodeURIComponent = window.encodeURIComponent;
-   exports.decodeURIComponent = window.decodeURIComponent;
-   exports.encodeURI = window.encodeURI;
-   exports.decodeURI = window.decodeURI;
- I will fix this madness at some point soon.
- start the build system - `scripts/watch.sh`
- start the development server - `scripts/server.sh`
- the project should be visible on http://localhost:8000


## Useful Commands
 - `~` - development panel
 - `|` - show fps
 - `1,2,3,..` - increment various components in component library
 - `q,w,e,..` - decrement various components. for instance, 1 choses the next transformation in the library, while q will chose the previous one
 - `a,s,d,..` - increase value of numerical paramenters
 - `z,x,c,..` - decreaes value of associated paramenter
