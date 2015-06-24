/*
  Template Haskell communication

  reads messages from stdin, sends over stderr
  (Haskell stderr stream is redirected to stdout)

  messages are prefixed with the following data:
    - UInt32BE: message length
    - UInt32BE: message target:
                  0: control message from compiler to TH server
                  n: response to request n
*/

// set this to true to record each message and the received JS code to a file
// you can then replay the TH session later with 'node thrunner.js replay'
var h$THRecord = // true ||
      !!process.argv['GHCJS_RECORD_TH'];

var h$THReplay = process.argv.length > 0 && process.argv[process.argv.length-1] === 'replay';

var h$TH = { nWaiters: 0
           , waiters: {}
           , data: null
           , requestId: 1
           , loadedSymbol: null
           , sendRequest: h$sendRequest
           , awaitMessage: h$awaitMessage
           , sendMessage: h$sendMessage
           , loadCode: h$loadCode
           , loadCodeStr: h$loadCodeStr
           , bufSize: h$bufSize
           };

global.h$TH = h$TH;
global.require = require;
global.module = module;

var h$THfs = require('fs');

// start listening
function h$initTH() {
    process.stdin.setEncoding('utf8');
    process.stderr.setEncoding('binary');
    process.on('uncaughtException', function(err) { console.log(err); });
    h$awaitMessageRaw(0, h$loadInitialCode);
    process.stdin.on('readable', function() {
        while(true) {
            var str = process.stdin.read();
            if(str) {
                var buf = new Buffer(str, 'hex');
                h$TH.data = h$TH.data ? Buffer.concat([h$TH.data, buf]) : buf;
                h$processQueue();
            } else {
                return;
            }
        }
    });
    process.stdin.on('close', function() { process.exit(0); });
}

var h$THMessageN = 0;
function h$processQueue() {
    while(h$TH.nWaiters > 0 && h$TH.data && h$TH.data.length >= 8) {
        var msgLength = h$TH.data.readUInt32BE(0);
        var msgTarget = h$TH.data.readUInt32BE(4);
        if(h$TH.data.length >= msgLength + 8 && h$TH.waiters[msgTarget]) {
            var w = h$TH.waiters[msgTarget], b = h$TH.data.slice(8, msgLength + 8);
            h$TH.data = h$TH.data.slice(msgLength + 8);
            delete h$TH.waiters[msgTarget];
            h$TH.nWaiters--;
            if(h$THRecord && !h$THReplay)
                h$THfs.writeFileSync("thmessage." + (++h$THMessageN) + ".dat", b);
            w(b);
        } else {
            return;
        }
    }
}

function h$sendRequest(bs, offset, len, c) {
    var req = h$TH.requestId++;
    h$sendMessage(bs, offset, len, req, function() {});
    h$awaitMessage(req, c);
}

function h$sendMessage(bs, offset, len, req, c) {
    var msg = len === -1 ? new Buffer(bs.u8.subarray(offset))
                         : new Buffer(bs.u8.subarray(offset, len));
    var hdr = new Buffer(8);
    hdr.writeUInt32BE(msg.length, 0);
    hdr.writeUInt32BE(req, 4);
    process.stderr.write(Buffer.concat([hdr, msg]), 'binary', function() { c(); });
}

function h$awaitMessage(req, c) {
    h$awaitMessageRaw(req, function(buf) {
        c(h$THWrapBuffer(h$BufferToArrayBuffer(buf),false),0);
    });
}

var h$THReplayMessageN = 0;
function h$awaitMessageRaw(req, c) {
    if(h$TH.waiters[req]) throw ("h$awaitMessage: already waiting for " + req);
    if(h$THReplay) {
        try {
            c(h$THfs.readFileSync('thmessage.' + (++h$THReplayMessageN) + '.dat'));
            return;
        } catch(e) { }
    }
    h$TH.nWaiters++;
    h$TH.waiters[req] = c;
    h$processQueue();
}

function h$bufSize(buf, buf_offset) {
    if(buf === null) return 0;
    return buf.len;
}

// load the RTS and start the server
function h$loadInitialCode(buf) {
    var code = buf.toString('utf8');
    h$loadCodeStr(code, true);
    // don't allow Haskell to read from stdin
    h$base_stdin_fd.read = function(fd, fdo, buf, buf_offset, n, c) { c(0); }
    // redirect Haskell's stderr to stdout since we use stderr to communicate
    h$base_stderr_fd.write = h$base_stdout_fd.write;
    h$main(h$ghcjszmprimZCGHCJSziPrimziTHziEvalzirunTHServer);

}

var h$THCodeN = 0;
function h$loadCodeStr(str, isFirst) {
    if(h$THReplay) {
        try {
            str = h$THfs.readFileSync("thcode." + (++h$THCodeN) + ".js").toString('utf8');
        } catch(e) { }
    } else if(h$THRecord) {
        h$THfs.writeFileSync("thcode." + (++h$THCodeN) + ".js", str);
    }
    eval.call(null, str);
}

// load additional code and run the initializers for it, the code should
// assign the h$TH.loadedSymbol variable
function h$loadCode(buf, off, len) {
    h$TH.loadedSymbol = null;
    var str = new Buffer(buf.u8).toString('utf8',off, off+len);
    h$TH.loadCodeStr(str, false);
    // h$runInitStatic();
    if(h$TH.loadedSymbol === null) throw "h$loadCode: error loading code"
    return h$TH.loadedSymbol;
}

function h$BufferToArrayBuffer(buf) {
    if(buf.toArrayBuffer) return buf.toArrayBuffer();
    return new Uint8Array(buf).buffer;
}

// copied from src/mem.js
function h$THWrapBuffer(buf, unalignedOk, offset, length) {
    if(!unalignedOk && offset && offset % 8 !== 0) {
        throw ("h$THWrapBuffer: offset not aligned:" + offset);
    }
    if(!buf || !(buf instanceof ArrayBuffer))
        throw "h$THWrapBuffer: not an ArrayBuffer"
    if(!offset) { offset = 0; }
    if(!length || length < 0) { length = buf.byteLength - offset; }
    // console.log("wrapping buf: " + length + " " + offset);
    return { buf: buf
             , len: length
             , i3: (offset%4) ? null : new Int32Array(buf, offset, length >> 2)
             , u8: new Uint8Array(buf, offset, length)
             , u1: (offset%2) ? null : new Uint16Array(buf, offset, length >> 1)
             , f3: (offset%4) ? null : new Float32Array(buf, offset, length >> 2)
             , f6: (offset%8) ? null : new Float64Array(buf, offset, length >> 3)
             , dv: new DataView(buf, offset, length)
           };
}

h$initTH();

