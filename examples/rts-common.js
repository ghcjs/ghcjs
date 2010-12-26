var $hs = {
    // treat signed integer as unsigned word, assuming complementary representation
    Word : {
      gt : function(a, b) {
        if ((a < 0 || b < 0) && (a >= 0 || b >= 0)) // Different signs
          return a < b;
        else
          return a > b;
      },
      ge : function(a, b) {
        if ((a < 0 || b < 0) && (a >= 0 || b >= 0)) // Different signs
          return a <= b;
        else
          return a >= b;
      },
      lt : function(a, b) {
        if ((a < 0 || b < 0) && (a >= 0 || b >= 0)) // Different signs
          return a > b;
        else
          return a < b;
      },
      le : function(a, b) {
        if ((a < 0 || b < 0) && (a >= 0 || b >= 0)) // Different signs
          return a >= b;
        else
          return a <= b;
      }
    },
    Int : { // binary operations supposed to work with 32bit words
      addCarry : function(a, b, c) {
         var word16addCarry = function(a, b, c) {
            var sum = a + b + c;
            var res = sum & 0xFFFF;
            var carry = sum >>> 16;
            return [res, carry];
         };
         var resl = word16addCarry(a & 0xFFFF, b & 0xFFFF, c);
         var resh = word16addCarry(a >>> 16, b >>> 16, resl[1]);
         return [(resh[0] << 16) | resl[0], resh[1]];
      },
      mul : function(a, b) {
         var al = a & 0xFFFF;
         var ah = a >>> 16;
         var bl = b & 0xFFFF;
         var bh = b >>> 16;
         var r = $hs.Int.addCarry(al * bh, bl * ah, 0)[0]
         return $hs.Int.addCarry(al * bl, (r & 0xFFFF) << 16, 0)[0];
      },
      mulIntMayOflo : function(a, b) {
        return a >>> 16 == 0 && b >>> 16 == 0;
      }
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
    }
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
$hs.loadPaths = ["./"];
$hs.packages = [".", "ghc-prim", "integer-simple", "base"];
$hs.loadModule = function (moduleName) {
	    variableName = moduleName.replace(/z/g, "zz").replace(/\./g, "zi"); // Z-encoding string
	    modulePath = moduleName.replace(/\./g, "/") + ".js";
	    if ($hs.modules[variableName] != undefined) {
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
		        if (transport.status == 200 || transport.status == 0)
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
};

$hs.fromHaskellString = function() {
          var res = "";
          var s = $hs.force.apply($hs, arguments);
          for (;;) {
            switch (s.tag) {
              case 1: // nil
                return res;
              case 2: // cons
                var chthunk = s.data[0];
                var sthunk = s.data[1];
                var ch = $hs.force(chthunk);
                res = res + ch.data[0];
                s = $hs.force(sthunk);
            }
          }
        };
$hs.fromHaskellInt = function() {
          var i = $hs.force.apply($hs, arguments);
          return i.data[0];
        };
$hs.fromHaskellIO = function() {
          var newArguments = [];
          for (var i = 0; i < arguments.length; i++)
            newArguments[i] = arguments[i];
          newArguments[arguments.length] = $hs.modules.GHCziPrim.hs_realWorldzh;
          var i = $hs.force.apply($hs, newArguments);
          return i[1];
        };
$hs.toHaskellInt = function(i) {
    var hsi = new $hs.Data(1);
    hsi.data = [(0 + i) & ~0];
    return hsi;
};
$hs.nil = function() {
    return "";
};
$hs.cons = function(x, xs) {
    return String.fromCharCode(x) + xs;
};

$hs.init = function() {
    $hs.modules.GHCziPrim = new $hs.Module();
    $hs.modules.GHCziPrim.dependencies = [];
    $hs.modules.GHCziPrim.initBeforeDependencies = function () {};
    $hs.modules.GHCziPrim.hs_realWorldzh = new $hs.Data(1);

    $hs.loadModule("GHC.Bool"); // Is required for primitive operations
}


