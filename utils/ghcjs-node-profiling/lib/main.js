var binding = null;

function isProfiling() {
    return process.execArgv.indexOf('--prof') > -1;
}

if(isProfiling()) {
    // add precompiled binaries for Windows?
    // if(process.platform == "win32" && process.arch == "x64") {
    //   binding = require('../bin/winx64/ghcjs-profiling');
    // } else {
    try {
      binding = require('../build/Release/ghcjs-profiling');
    } catch(e) {
      binding = require('../build/Debug/ghcjs-profiling');
    }
    // }

    var arr = [ binding.call1,  binding.call2,  binding.call3,  binding.call4
              , binding.call5,  binding.call6,  binding.call7,  binding.call8
              , binding.call9,  binding.call10, binding.call11, binding.call12
              , binding.call13, binding.call14, binding.call15, binding.call16
              ];
  binding.initcc();
  module.exports =
    { registerCC:
        function(n, name, file, line, col) {
          binding.addcc(n|0, ''+name, ''+file, line|0, col|0);
        }
    , registerCCS:
        function(n, ccArray) {
          binding.addccs(n|0, ccArray);
        }
    , setCCS:
        function(n) {
          binding.setccs(n|0);
        }
    , runCC:
        function(f) {
          if(typeof f !== 'function')
            throw new Error("argument must be a function");
          binding.runcc(f, arr);
        }
      , isProfiling: function() { return true; }
    };
} else {
    module.exports =
      { registerCC:  function() {}
      , registerCCS: function() {}
      , setCCS:      function() {}
      , runCC:       function(f) { f(); }
      , isProfiling: function() { return false; }
      };
}
