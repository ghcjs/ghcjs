/*
   print what symbols a script makes in the top-level scope
   use this to check for unprefixed names

   also try running your program with --use_strict to find undeclared globals
 */

var vm = require('vm');
var fs = require('fs');

var what = 'nonhs';
var script;
var wrap = false;
var opts = [];
var nonOpts = [];
for(var i=2;i<process.argv.length;i++) {
    var a = process.argv[i];
    if(a.substring(0,2) == '--') opts.push(a); else nonOpts.push(a);
}
if(nonOpts.length === 1) script = getScript(nonOpts[0]); else usage();
opts.forEach(function(opt) {
    if(opt === '--wrap') wrap = true;
    else if(opt === '--all') what = 'all';
    else if(opt === '--public') what = 'public';
    else usage();
});

function getScript(name) {
    return fs.readFileSync(name);
}

function usage() {
    console.log(['usage', process.argv[0], process.argv[1],'[option]','script.js'].join(' '));
    console.log(['\n  options'
                 ,'--wrap:   wrap script in function'
                 ,'--all:    print all names, not just those not prefixed with h$'
                 ,'--public: print all public names (that do not start with h$$)'].join('\n    '));
    process.exit(1);
}

/*
   give our script a process object, but process.exit shouldn't
   really exit the process, just stop the scheduler
 */
var ctxProcess = copy(process);
ctxProcess.exit = function(exitCode) {
    this.disableTimeout = true;
}
ctxSetTimeout = function() {
    if(!ctxProcess.disableTimeout)
        return setTimeout.apply(this, arguments);
}
ctxProcess.stdin = null;
ctxProcess.disableTimeout = false;

var ctx = { global: global
          , process: ctxProcess
          , console: console
          , require: require
          , setTimeout: ctxSetTimeout
          , clearTimeout: clearTimeout
          , setInterval: setInterval
          , clearInterval: clearInterval
          , Buffer: Buffer
          , ArrayBuffer: ArrayBuffer
          , DataView: DataView
          , Int8Array: Int8Array
          , Uint8Array: Uint8Array
          , Uint8ClampedArray: Uint8ClampedArray
          , Int16Array: Int16Array
          , Uint16Array: Uint16Array
          , Int32Array: Int32Array
          , Uint32Array: Uint32Array
          , Float32Array: Float32Array
          , Float64Array: Float64Array
          };

function copy(o) {
    var q = {};
    for(var p in o) q[p] = o[p];
    return q;
}

var ctxBefore = copy(ctx);

console.log('*** starting script');

var vmCtx = vm.createContext(ctx);
if(wrap) script = '(function() {\n' + script + '})();'
vm.runInContext(script, vmCtx, '<script>');
reportGlobals(ctxBefore, vmCtx);

function reportGlobals(b, c) {
    console.log('*** script run, collected properties:');
    for(var p in c) {
        if(c.hasOwnProperty(p) && !b.hasOwnProperty(p)) {
            if(what === 'all' ||
               (what === 'public' && p.substring(0,3) !== 'h$$') ||
               (what === 'nonhs' && p.substring(0,2) !== 'h$'))
                console.log('  - ' + p);
        }
    }
}


