/*
  GHCJSi communication

  reads messages from stdin, sends over stderr
*/

var h$GHCJSiRecord = // true ||
      !!process.env['GHCJS_RECORD_GHCJSI'];

var h$GHCJSiPort = process.env['GHCJSI_PORT'] || 6400;

var h$GHCJSiReplay = process.argv.length > 0 &&
                     process.argv[process.argv.length-1] === 'replay';

var h$GHCJSi = { data: null
               , loadedSymbols: {}
               , current: null
               , sendMessage: h$sendMessage
               , done: h$GHCJSiDone
		 , clientHtml: ''
		 , clientJS: ''
	 , socket: null
               };

global.h$GHCJSi = h$GHCJSi;
global.require  = require;
global.module   = module;

var fs     = require('fs');
var server = require('http').createServer(h$handleHTTP);
var url    = require('url');
var io = null;
try {
  io = require('socket.io')(server);
} catch (e) { }

// start listening
function h$initGHCJSi() {
  process.stdin.setEncoding('utf8');
  process.stderr.setEncoding('binary');
  process.on('uncaughtException', function(err) {
    console.log(err);
    console.log(err.stack);
  });
  if(h$GHCJSiReplay) {
    h$replayMessage(1);
  } else {
    h$startHTTPServer();
    process.stdin.on('readable', function() {
      while(true) {
	var str = process.stdin.read();
	if(str) {
        var buf = new Buffer(str, 'hex');
          h$GHCJSi.data = h$GHCJSi.data ? Buffer.concat([h$GHCJSi.data, buf]) : buf;
          h$processInput();
	} else {
          return;
	}
      }
    });
    process.stdin.on('close', function() { process.exit(0); });
  }
}

function h$replayMessage(n) {
  try {
    var buffer  = fs.readFileSync("ghcjsimessage." + n + ".dat");
    var msgType = buffer.readUInt32BE(0);
    h$processMessage(msgType, buffer.slice(4));
    setTimeout(function() { h$replayMessage(n+1); }, 1500);
  } catch(e) { }
}

function h$startHTTPServer() {
  if(!io) {
    console.log("\nsocket.io not found, browser session not available");
    return;
  }
  io.on('connection', function(socket) {
    console.log("\nbrowser connected, code runs in browser from now on");
    h$GHCJSi.socket = io;
    socket.on('msg', function(msg) {
      h$GHCJSi.sendMessage(msg.type, msg.payload);
    });
    socket.on('out', function(data) {
      process.stdout.write(data);
    });
    socket.on('disconnect', function() {
      // console.log('browser disconnected');
    });
  });
  h$GHCJSi.clientHtml = fs.readFileSync(__dirname + '/ghcjsiClient.html');
  h$GHCJSi.clientJs   = fs.readFileSync(__dirname + '/ghcjsiClient.js');
  server.listen(h$GHCJSiPort);
}

function h$handleHTTP(req, res) {
  var u = url.parse(req.url);
  if(u.pathname === '/' || u.pathname === '/index.html') {
    res.writeHead(200, { 'Content-Type': 'text/html' });
    res.end(h$GHCJSi.clientHtml);
  } else if(u.pathname === '/client.js') {
    res.writeHead(200, { 'Content-Type': 'application/javascript' });
    res.end(h$GHCJSi.clientJs);
  } else {
    res.statusCode = 404;
    res.statusMessage = 'not found';
    res.end();
  }
}

var h$GHCJSiMessageN = 0;
function h$processInput() {
  while(h$GHCJSi.data && h$GHCJSi.data.length >= 8) {
    var msgLength = h$GHCJSi.data.readUInt32BE(0);
    var msgType = h$GHCJSi.data.readUInt32BE(4);
    if(h$GHCJSi.data.length >= msgLength + 8) {
      if(h$GHCJSiRecord && !h$GHCJSiReplay) {
	fs.writeFileSync("ghcjsimessage." + (++h$GHCJSiMessageN) + ".dat"
			 ,h$GHCJSi.data.slice(4, msgLength+8));
      }
      var msgPayload = h$GHCJSi.data.slice(8, msgLength + 8);
      h$GHCJSi.data = h$GHCJSi.data.slice(msgLength + 8);
      if(h$GHCJSi.socket) {
	h$GHCJSi.socket.emit('msg', { type: msgType, payload: msgPayload });
      } else {
	h$processMessage(msgType, msgPayload);
      }
    } else return;
  }
}

function h$processMessage(msgType, msgPayload) {
  switch(msgType) {
  case 0: // load initial code/rts and init
    h$loadInitialCode(msgPayload.toString('utf8'));
    h$sendMessage(0);
    break;
  case 1: // load code
    h$loadCodeStr(msgPayload.toString('utf8'));
    h$sendMessage(0);
    break;
  case 2: // run action
    var symb = msgPayload.toString('utf8');
    h$GHCJSi.current = h$main(h$GHCJSi.loadedSymbols[msgPayload.toString('utf8')]);
    break;
  case 3: // abort
    if(h$GHCJSi.current)
      h$killThread( h$GHCJSi.current
                  , h$baseZCControlziExceptionziBasezinonTermination);
    break;
  default:
    throw new Error("unknown message type: " + msgType);
  }
}

function h$GHCJSiDone(thread) {
  h$sendMessage(0);
  h$GHCJSi.current = null;
}

function h$sendMessage(msgType, msg, c) {
  var hdr = new Buffer(8);
  hdr.writeUInt32BE(msg ? msg.length : 0, 0);
  hdr.writeUInt32BE(msgType, 4);
  process.stderr.write( msg ? Buffer.concat([hdr, msg]) : hdr, 'binary'
                      , function() { if(c) c(); });
}

// load the RTS and set up the standard streams
function h$loadInitialCode(code) {
  h$loadCodeStr(code, true);

  // don't allow Haskell to read from stdin (fixme!)
  h$base_stdin_fd.read = function(fd, fdo, buf, buf_offset, n, c) { c(0); }
  
  // redirect Haskell's stderr to stdout since we use stderr to communicate (fixme!)
  h$base_stderr_fd.write = h$base_stdout_fd.write;
}

function h$loadCodeStr(str) {
  eval.call(null, str);
}

h$initGHCJSi();
