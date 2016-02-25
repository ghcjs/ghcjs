var h$GHCJSiSocket = io();
var global = window;

var h$GHCJSi = { socket: io()
		 , out: function(dat) {
		   h$GHCJSi.socket.emit('out', dat);
		 }
		 , msg: function(msgType, msgPayload) {
		   if(!msgPayload) msgPayload = new ArrayBuffer(0);
		   h$GHCJSi.socket.emit('msg', { type: msgType, payload: msgPayload });
		 }
		 , current: null
		 , loadedSymbols: {}
		 , done: function(thread) {
		   h$GHCJSi.msg(0);
		   h$GHCJSi.current = null;
		 }
	       };

h$GHCJSi.socket.on('msg', function(msg) {
  h$processMessage(msg.type, msg.payload);
});

function h$processMessage(msgType, msgPayload) {
  // console.log("processMessage: " + msgType);
  switch(msgType) {
  case 0: // load initial code/rts and init
    h$loadInitialCode(h$decodeUtf8(h$wrapBuffer(msgPayload)));
    h$GHCJSi.msg(0);
    break;
  case 1: // load code
    h$loadCodeStr(h$decodeUtf8(h$wrapBuffer(msgPayload)));
    h$GHCJSi.msg(0);
    break;
  case 2: // run action
    var symb = h$decodeUtf8(h$wrapBuffer(msgPayload));
    h$GHCJSi.current = h$main(h$GHCJSi.loadedSymbols[symb]);
    break;
  case 3: // abort
    if(h$GHCJSi.current)
      // fixme should probably be wrapped with Exception dict?
      h$killThread( h$GHCJSi.current
                    , h$baseZCControlziExceptionziBasezinonTermination);
    break;
  default:
    throw new Error("unknown message type: " + msgType);
  }
}

function h$loadInitialCode(code) {
  h$loadCodeStr(code, true);

  // don't allow Haskell to read from stdin (fixme!)
  h$base_stdin_fd.read = function(fd, fdo, buf, buf_offset, n, c) { c(0); }
  
  // redirect Haskell's stderr to stdout since we use stderr to communicate (fixme!)
  h$base_stdout_fd.write = function(fd, fdo, buf, buf_offset, n, c) {
    h$GHCJSi.out(buf.buf.slice(buf_offset, buf_offset+n));
    c(n);
  }
  h$base_stderr_fd.write = h$base_stdout_fd.write;
}

function h$loadCodeStr(str) {
  eval.call(null, str);
}

/////////////////////////////////////////////////////////////////////////
// UTF-8 functions from shims/src/string.js and shims/src/mem.js
/////////////////////////////////////////////////////////////////////////
    
function h$wrapBuffer(buf, unalignedOk, offset, length) {
  if(!unalignedOk && offset && offset % 8 !== 0) {
    throw ("h$wrapBuffer: offset not aligned:" + offset);
  }
  if(!buf || !(buf instanceof ArrayBuffer))
    throw "h$wrapBuffer: not an ArrayBuffer"
  if(!offset) { offset = 0; }
  if(!length || length < 0) { length = buf.byteLength - offset; }
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
    
// decode a buffer with Utf8 chars to a JS string
// invalid characters are ignored
function h$decodeUtf8(v,n0,start) {
  var n = n0 || v.len;
  var arr = [];
  var i = start || 0;
  var code;
  var u8 = v.u8;
  while(i < n) {
    var c = u8[i];
    while((c & 0xC0) === 0x80) {
      c = u8[++i];
    }
    if((c & 0x80) === 0) {
      code = (c & 0x7F);
      i++;
    } else if((c & 0xE0) === 0xC0) {
      code = ( ((c & 0x1F) << 6)
             | (u8[i+1] & 0x3F)
             );
      i+=2;
    } else if((c & 0xF0) === 0xE0) {
      code = ( ((c & 0x0F) << 12)
             | ((u8[i+1] & 0x3F) << 6)
             | (u8[i+2] & 0x3F)
             );
      i+=3;
    } else if ((c & 0xF8) === 0xF0) {
      code = ( ((c & 0x07) << 18)
             | ((u8[i+1] & 0x3F) << 12)
             | ((u8[i+2] & 0x3F) <<  6)
             | (u8[i+3] & 0x3F)
             );
      i+=4;
    } else if((c & 0xFC) === 0xF8) {
      code = ( ((c & 0x03) << 24)
             | ((u8[i+1] & 0x3F) << 18)
             | ((u8[i+2] & 0x3F) << 12)
             | ((u8[i+3] & 0x3F) <<  6)
             | (u8[i+4] & 0x3F)
             );
      i+=5;
    } else {
      code = ( ((c & 0x01) << 30)
             | ((u8[i+1] & 0x3F) << 24)
             | ((u8[i+2] & 0x3F) << 18)
             | ((u8[i+3] & 0x3F) << 12)
             | ((u8[i+4] & 0x3F) <<  6)
             | (u8[i+5] & 0x3F)
             );
      i+=6;
    }
    if(code > 0xFFFF) {
      var offset = code - 0x10000;
      arr.push(0xD800 + (offset >> 10), 0xDC00 + (offset & 0x3FF));
    } else {
      arr.push(code);
    }
  }
  return h$charCodeArrayToString(arr);
}

function h$charCodeArrayToString(arr) {
    if(arr.length <= 60000) {
	return String.fromCharCode.apply(this, arr);
    }
    var r = '';
    for(var i=0;i<arr.length;i+=60000) {
	r += String.fromCharCode.apply(this, arr.slice(i, i+60000));
    }
    return r;
}
