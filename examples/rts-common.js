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
    },
    MutVar : {
        newMutVar : function(a, s) {
            return [s, {value : a}];
        },
        read : function (a, s) {
            return [s, a.value];
        },
        write : function (a, b, s) {
            a.value = b;
            return [s];
        },
        same : function (a, b, s) {
            return [s, a === b];
        }
    },
    _Array : {
        newArray : function(n, a, s) {
            var result = [];
            for (x = 0; x != n; x++)
              result[x] = a;
            return [s, result];
        },
        same : function (a, b, s) {
            return [s, a === b];
        },
        read : function (a, n, s) {
            return [s, a[n]];
        },
        write : function (a, n, b, s) {
            a[n] = b;
            return [s];
        },
        sizeof : function (a, s) {
            return [s, a.length];
        },
        sizeofMut : function (a, s) {
            return [s, a.length];
        },
        index : function (a, n) {
            return a[n];
        },
        unsafeFreeze : function (a, s) {
            return [s, a];
        },
        unsafeThaw : function (a, s) {
            return [s, a];
        }
    }
}

$hs.fromHaskellString = function() {
  var res = "";
  var s = $hs.force.apply($hs, arguments);
  for (;;) {
    switch (s.g) {
      case 1: // nil
        return res;
      case 2: // cons
        var chthunk = s.v[0];
        var sthunk = s.v[1];
        var ch = $hs.force(chthunk);
        res = res + ch.v[0];
        s = $hs.force(sthunk);
        break;
      default:
        return undefined;
    }
  }
};
$hs.fromHaskellInt = function() {
    console.log("fromHaskellInt");
    var i = $hs.force.apply($hs, arguments);
    return i.v[0];
};
$hs.fromHaskellIO = function() {
    console.log("fromHaskellIO");
    var newArguments = [];
    for (var i = 0; i < arguments.length; i++)
    newArguments[i] = arguments[i];
    newArguments[arguments.length] = $$GHCziPrim_realWorldzh;
    var i = $hs.force.apply($hs, newArguments);
    return i[1];
};
$hs.toHaskellInt = function(i) {
    console.log("toHaskellInt");
    var hsi = new $Data(1);
    hsi.v = [(0 + i) & ~0];
    return hsi;
};
$hs.nil = function() {
    return "";
};
$hs.cons = function(x, xs) {
    return String.fromCharCode(x) + xs;
};

$hs.init = function() {
    $$GHCziPrim_realWorldzh = $D(1, function(){return []}, "realWorld#");
}

