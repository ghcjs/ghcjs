var $hs = {
    Word : {
      gt : function(a, b) {
        if ((a < 0 || b < 0) && (a >= 0 || b >= 0)) // Different signs
          return a > b ? $hs.modules.GHCziBool.hs_False : $hs.modules.GHCziBool.hs_True;
        else
          return a > b ? $hs.modules.GHCziBool.hs_True : $hs.modules.GHCziBool.hs_False;
      },
      ge : function(a, b) {
        if ((a < 0 || b < 0) && (a >= 0 || b >= 0)) // Different signs
          return a >= b ? $hs.modules.GHCziBool.hs_False : $hs.modules.GHCziBool.hs_True;
        else
          return a >= b ? $hs.modules.GHCziBool.hs_True : $hs.modules.GHCziBool.hs_False;
      },
      lt : function(a, b) {
        if ((a < 0 || b < 0) && (a >= 0 || b >= 0)) // Different signs
          return a < b ? $hs.modules.GHCziBool.hs_False : $hs.modules.GHCziBool.hs_True;
        else
          return a < b ? $hs.modules.GHCziBool.hs_True : $hs.modules.GHCziBool.hs_False;
      },
      le : function(a, b) {
        if ((a < 0 || b < 0) && (a >= 0 || b >= 0)) // Different signs
          return a <= b ? $hs.modules.GHCziBool.hs_False : $hs.modules.GHCziBool.hs_True;
        else
          return a <= b ? $hs.modules.GHCziBool.hs_True : $hs.modules.GHCziBool.hs_False;
      },
    },
    modules: {},
    alert: function (str) {
        window.alert(str);
    },
    logAny: function (c, str) {
        var el = document.getElementById('log');
        el.innerHTML = el.innerHTML + c + ": " + str + '<br/>\n';
    },
    logInfo: function (str) {
        $hs.logAny("INFO", str);
    },
    logError: function (str) {
        $hs.logAny("ERROR", str);
    },
    logDebug: function (str) {
        $hs.logAny("DEBUG", str);
    },
    loadPaths: ["./"],
    packages: [".", "ghc-prim", "integer-simple", "base"],
    loadModule: function (moduleName) {
	    variableName = moduleName.replace (/\./g, "zi"); // Z-encoding string
	    modulePath = moduleName.replace (/\./g, "/") + ".js";
	    if ($hs.modules[moduleName] != undefined) {
		$hs.logInfo("Module already loaded: " + moduleName + ": skipping");
		return;
	    }

	    var code = null;
	    for (var i = 0; i < $hs.loadPaths.length && code == null; i++) {
	        for (var j = 0; j < $hs.packages.length && code == null; j++) {
		    var path = $hs.loadPaths[i] + $hs.packages[j] + "/" + modulePath;
		    try {
		        var transport = new XMLHttpRequest();
		        transport.open("GET", path, false);
		        transport.send(null);
		        code = transport.responseText;
		    } catch (e) { }
	        }
            }
	    try {
		eval(code);
		$hs.modules[variableName].init();
	    } catch (e) {
		$hs.logError("Error evaluating module: " + moduleName + ":\n" + e);
		return false;
	    }
	    
	    return true;
    },
    hscall : function () {
	if (this.arity == arguments.length) { // EXACT and THUNK rules
		return this.evaluate.apply(this, arguments);
	} else if (this.arity < arguments.length) { // CALLK and TCALL rules
		var remainingArguments = Array.prototype.slice.call(arguments, this.arity, arguments.length);
		arguments.length = this.arity;
		var result = this.evaluate.apply(this, arguments);
		return result.hscall.apply(result, remainingArguments);
	} else if (arguments.length == 0) { // RETFUN
		return this;
    } else if (this instanceof $hs.Pap) { // PCALL rule, we can bypass this rule by building PAPs of PAPs
		return this.evaluate.apply(this, arguments);
	} else {
		// PAP2 rule and then RETFUN (jump to continuation)
		return new $hs.Pap (this, arguments);
        }
    },
}

$hs.Module = function () {};
$hs.Module.prototype = {
    init: function () {
        this.initBeforeDependencies();
    },
    loadDependencies: function () {
        for (var i = 0; i < this.dependencies.length; i++)
	    $hs.loadModule(this.dependencies[i]);
        this.initAfterDependencies();
    }
};

$hs.Pap = function(obj, args) {
    this.arity = obj.arity - args.length;
    this.object = obj;
    this.savedArguments = args;
};
$hs.Pap.prototype = {
    hscall: $hs.hscall,
    evaluated: true,
    evaluate: function () {
            var k = arguments.length;
            var n = this.savedArguments.length;
            var newArguments = new Array (k + n);
            for (var i = 0; i < n; i++)
                newArguments[i] = this.savedArguments[i];
            for (var i = 0; i < k; i++)
                newArguments[n + i] = arguments[i];
            return this.object.hscall.apply(this.object, newArguments);
    }
};

$hs.Func = function(a) {
    this.arity = a;
};
$hs.Func.prototype = {
    hscall: $hs.hscall,
    evaluated: true,
};

$hs.Thunk = function() {};
$hs.Thunk.prototype = {
    hscall: $hs.hscall,
    arity: 0,
    evaluated: false,
    evaluate: function() {
        var res = this.evaluateOnce();
        this.evaluate = function () { return res; };
        return res;
    }
};

$hs.Data = function (t) {
    this.tag = t;
};
$hs.Data.prototype = {
    hscall: $hs.hscall,
    arity: 0,
    evaluated: true,
    evaluate: function() {
        return this;
    }
};

$hs.modules.GHCziPrim = new $hs.Module();
$hs.modules.GHCziPrim.dependencies = [];
$hs.modules.GHCziPrim.initBeforeDependencies = function () {};
$hs.modules.GHCziPrim.hs_realWorldzh = new $hs.Data(1);

$hs.fromHaskellString = function(sthunk) {
          var res = "";
          for (;;) {
            var s = sthunk.hscall();
            switch (s.tag) {
              case 1: // nil
                return res;
              case 2: // cons
                var chthunk = s.data[0];
                var sthunk = s.data[1];
                var ch = chthunk.hscall();
                res = res + ch.data[0];
            }
          }
        };
$hs.fromHaskellInt = function(ithunk) {
          var i = ithunk.hscall();
          return i.data[0];
        };

