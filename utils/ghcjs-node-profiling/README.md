### GHCJS Profiling tools

This package contains tools to improve profiling information for GHCJS programs running
on node.js. It consists of a slighly modified tick processor script, based on the one
in the v8 source distribution, and a native node.js addon to inject cost centre information
into the samples.

### How to use

install the package:

```
$ npm install . -g
```

Make sure the installation path is in your `NODE_PATH`. Usually this means adding
`$HOME/lib/node_modules`. The following should not print an error:

```
$ node -e "require('ghcjs-profiling')"
```

Compile your Haskell program with profiling:

```
$ cabal configure --ghcjs --enable-profiling
$ cabal build
```

now run it with node, with profiling enabled:

```
$ node --prof ./dist/buid/myProgram/myProgram.jsexe/all.js
```

If this package has been installed, two log files will be produced:

  - `v8.log`: Information from the V8 JS engine, code locations and collected samples.
  - `v8.log.ghcjs`: GHCJS-specific information, cost centre names and stacks.

Generate a profiling report from the log files by running

```
$ ghcjs-tick-processor
```
