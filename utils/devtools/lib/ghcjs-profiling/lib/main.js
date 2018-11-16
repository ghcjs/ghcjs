var binding = null;
var isElectron = require('is-electron');
const nCostCentreFuns = 1024;

function isProfiling() {
    // return isElectron() || process.execArgv.indexOf('--prof') > -1;
    return true;
}

/*
   the backend wil retrieve the contents of these arrays when
   building the profile
 */
var h$_prof_costCentres = [];
var h$_prof_costCentreStacks = [];

if(typeof global !== 'undefined') {
  global.h$_prof_costCentres = h$_prof_costCentres;
  global.h$_prof_costCentreStacks = h$_prof_costCentreStacks;
} else if(typeof window !== 'undefined') {
  window.h$_prof_costCentres = h$_prof_costCentres;
  window.h$_prof_costCentreStacks = h$_prof_costCentreStacks;
}

/*
  Each function needs to be optimzed before it can be used by the profiler.
  We rely on %OptimizeFunctionOnNextCall for this.

  This requires v8 natives syntax to be enabled via the --allow-natives-syntax
  command line option. We keep this function as string, so that we can
  eval() it later and have a change to catch the parse error.
 */
var mkCostCentreFun_src =
[ "(function(f) {"
, "  var dummyFunction1 = function() { return 0; };"
, "  var dummyFunction2 = function() { return \"dummy\"; };"
, "  var dummyFunction3 = function() { return undefined; };"
, "  var dummyFunction4 = function() { return new Error().stack; };"
, "  f(dummyFunction1);"
, "  f(dummyFunction2);"
, "  f(dummyFunction3);"
, "  f(dummyFunction4);"
, "  %OptimizeFunctionOnNextCall(f);"
, "  f(dummyFunction1);"
, "  f(dummyFunction2);"
, "  f(dummyFunction3);"
, "  f(dummyFunction4);"
, "})"
];
/*
  Generate the functions with the names that the native extension recognizes.

 */
function mkCostCentreFuns() {
  try {
    var src = mkCostCentreFun_src.join('\n');
    console.log(src);
    var mkCostCentreFun = eval(src);
    var defs = [];
    var i;
    for(i = 0; i < 3; i++) {
      defs.push("function h$_prof_ccs_a_" + i + "(f) { return f(); }")
    }
    for(i = 0; i < nCostCentreFuns; i++) {
      defs.push("function h$_prof_ccs_b_" + i + "(f) { return f(); }")
    }
    var funs = eval('[' + defs.join(',\n') + ']');

    for(i = 0; i < funs.length; i++) {
      mkCostCentreFun(funs[i]);
    }
    return funs;
  } catch(e) {
    throw e; // new Error("Could not make cost centre functions, did you allow natives syntax?");
  }
}

if(isProfiling()) {
  try {
    binding = require('../build/Debug/ghcjs_profiling');
    // binding = require('../build/Release/ghcjs-profiling');
  } catch(e) {
    binding = require('../build/Release/ghcjs_profiling');
    // binding = require('../build/Debug/ghcjs-profiling');
  }
  if(!binding) {
    throw new Error("could not load ghcjs_profiling extension");
  } else {

    var profFuns = mkCostCentreFuns();
    var ccsBuf = new Uint32Array(binding.get_ccs_buffer());

    module.exports =
      {  /** register a cost centre so that devtools can display its
             information */
         registerCC: function(id, name, file, line, col) {
            h$_prof_costCentres.push({ id: id
                                     , name: name
                                     , file: file
                                     , line: line
                                     , col: col
                                     });

          }
        /** register a cost centre stack so that devtools can display its
            information */
        , registerCCS: function(id, ccArray) {
            h$_prof_costCentreStacks.push({ id: id
                                          , ccs: ccArray
                                          });
          }
        /** set the current cost centre stack id. */
        , setCCS: function(n) { ccsBuf[0] = n; }
        , isProfiling: function() { return true; }
        /** run a function under cost centre profiling */
        , runCC: function(f) {
          if(typeof f !== 'function')
            throw new Error("argument must be a function");

          // do our call through the three magic call-throughs
          return profFuns[2](function() {
            return profFuns[1](function() {
              return profFuns[0](function() {
                return f();
              });
            });
          });
        }
      };
  }
} else {
  module.exports =
    { registerCC:  function() {}
    , registerCCS: function() {}
    , setCCS:      function() {}
    , runCC:       function(f) { return f(); }
    , isProfiling: function() { return false; }
    };
}
