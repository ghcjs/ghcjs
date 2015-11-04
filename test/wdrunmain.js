/*
   WebDriver test harness, to be called asynchronously with arguments:
     0:     program command-line arguments
     1:     base URL for loading files
     (n-1): callback for signalling completion, returns testcase result:
              { exit: exit code
              , out:  data written to stdout
              , err:  data written to stderr
              }
 */

function h$fileUrl(baseUrl, currentDir, file) {
  if(file.substr(0,1) === '/') {
    return baseUrl + file;
  } else {
    if(currentDir.substr(0,1) !== '/') {
      currentDir = currentDir.substr(1);
    }
    if(currentDir === '' || currentDir.substr(-1) === '/') {
      return baseUrl + currentDir + file;
    } else {
      return baseUrl + currentDir + '/' + file;
    }
  }
}

function h$runMainWebDriver(args, baseUrl, cb) {
  // set up command line arguments
  var a = args.slice(0);
  a.unshift("a.js");
  h$programArgs = a;

  // redirect stderr and stdout to our own buffers, so we can
  // report the result to TestRunner
  var wdStderr = [];
  var wdStdout = [];
  h$base_writeStdout = function(fd, fdo, buf, buf_offset, n, c) {
    wdStdout.push(h$decodeUtf8(buf, n, buf_offset));
    c(n);
  };
  h$base_writeStderr = function(fd, fdo, buf, buf_offset, n, c) {
    wdStderr.push(h$decodeUtf8(buf, n, buf_offset));
    c(n);
  };
  h$log = function() {
    var s = '';
    for(var i=0;i<arguments.length;i++) { s = s + arguments[i]; }
    wdStdout.push(s+'\n');
  };
  h$errorMsg = function(pat) {
    // poor man's vprintf
    var str = pat;
    for(var i=1;i<arguments.length;i++) {
      str = str.replace(/%s/, arguments[i]);
    }
    wdStderr.push(str);
  };
  h$base_stdout_fd.write = h$base_writeStdout;
  h$base_stderr_fd.write = h$base_writeStderr;

  // keep track of the current dir, for resolving file urls
  h$directory_getCurrentDirectory  = function() { return currentDir; };
  h$directory_setCurrentDirectory  = function(dir) { currentDir = dir; return 0; };
  h$directory_getDirectoryContents = function(dir, c) { c([]); };

  // read files from server, write changes locally
  var files      = [];  // fd   -> { data, length }
  var paths      = {};                // path -> fd
  var currentDir = "/";
  function returnNewFd(pu, how, c) {
    var fd = h$base_fds.length;
    var dat = files[pu];
    var off = (how & h$base_o_append) ? dat.length : 0;
    h$base_fds[fd] = { read:  readServerFile
                     , write: writeServerFile
                     , close: closeServerFile
                     , pos:   off
                     , data:  dat
                     };
    c(fd);
  }
  /*
   fixme, still missing:
     - h$base_stat, h$base_lstat, h$base_access
   */
  h$base_lseek = function(fd, pos_1, pos_2, whence, c) {
    var p = goog.math.Long.fromBits(pos_2, pos_1), p1;
    var o = h$base_fds[fd];
    if(!o) {
      h$setErrno('EBADF');
      c(-1,-1);
    } else {
      switch(whence) {
      case 0: /* SET */
        o.pos = p.toNumber();
        c(p.getHighBits(), p.getLowBits());
        break;
      case 1: /* CUR */
        o.pos += p.toNumber();
        p1 = goog.math.Long.fromNumber(o.pos);
        c(p1.getHighBits(), p1.getLowBits());
        break;
      case 2: /* END */
        o.pos = o.data.length + p.toNumber();
        p1 = goog.math.Long.fromNumber(o.pos);
        c(p1.getHighBits(), p1.getLowBits());
        break;
      default:
        h$setErrno('EINVAL');
        c(-1,-1);
      }
    }
  };
  h$base_fstat = function(fd, stat, stat_off, c) {
    var f = h$base_fds[fd];
    if(f) {
      h$base_fillStat({ mode: 292
                        , ino: 0
                        , uid: 0
                        , gid: 0
                        , dev: 0
                        , size: f.data.length
                      }
                      , stat
                      , stat_off);
      c(0);
    } else {
      h$setErrno('ENOENT');
      c(-1);
    }
  };
  h$base_open = function(file, file_off, how, mode, c) {
    var fp = h$decodeUtf8z(file, file_off);
    var u  = h$fileUrl(baseUrl, currentDir, fp);
    var pu = paths[u];
    if(typeof pu === 'number') {
      // file already loaded
      returnNewFd(pu, how, c);
    } else {
      var x = new XMLHttpRequest();
      x.addEventListener("load", function() {
        // file loaded now
        if(x.response) {
          var fl = files.length;
          files[fl] = { data: new Uint8Array(x.response)
                      , length: x.response.byteLength
                      };
          paths[u] = fl;
          returnNewFd(fl, how, c);
        } else {
          h$setErrno('ENOENT');
          c(-1);
        }
      });
      x.addEventListener("error", function() {
        h$setErrno('ENOENT');
        c(-1);
      });
      x.open("GET", u);
      x.responseType = "arraybuffer";
      x.send();
    }
  };

  var readServerFile = function(fd, fdo, buf, buf_offset, n, c) {
    var p = fdo.pos;
    var toRead = Math.min(n, fdo.data.length - p);
    if(toRead > 0) {
      for(var i=0;i<toRead;i++) {
        buf.u8[buf_offset+i] = fdo.data.data[p+i];
      }
      fdo.pos += toRead;
      c(toRead);
    } else {
      c(0);
    }
  };
  var writeServerFile = function(fd, fdo, buf, buf_offset, n, c) {
    var d = fdo.data.data;
    var p = fdo.pos;
    var i;
    // extend buffer if necessary
    if(p + n > d.length) {
      var newLength = Math.max(p+n, d.length * 1.5);
      var nd = new Uint8Array(newLength);
      for(i=0;i<d.length;i++) {
        nd[i] = d[i];
      }
      d = nd;
      fdo.data = nd;
    }
    for(i=0;i<n;i++) {
      buf.u8[buf_offset+i] = d[p+i];
    }
    fdo.pos += n;
    // update file size if necessary
    fdo.data.length = Math.max(fdo.data.length, p + n);
    c(n);
  };
  var closeServerFile = function(fd, fdo, c) {
    c(0); // keep local changes
  };

  // report result to testrunner when exitProcess is called
  var oldExitProcess = h$exitProcess;
  h$exitProcess = function(code) {
    cb ({ exit: code
        , err:  wdStderr.join('')
        , out:  wdStdout.join('')
        });
    oldExitProcess(code);
  };

  // finally start the main action
  h$main(h$mainZCZCMainzimain);
}
