# Running a GHCJS program in Electron

This is a simple package to load a GHCJS program into an Electron window and
expose additional diagnostics utilities that rely on the node.js API.

## Using

Compile the Haskell program with profiling turned on.

```bash
# Install dependencies
npm install
# Run the app
npm start http://host/app.jsexe/
```

## Stack Limit

There is currently a hardcoded limit in the number of cost centre stack
frames that can be displayed. If the stack is longer, the remaining
cost centres are squeezed into a single frame.

The names are preserved, but the file name and line number of the squeezed
cost centres are lost.

The limit can be changed by adjusting the value of the `nFrames` constant
in `lib/ghcjs-profiling/lib/main.js`
