var $hs_hscall = function () {
    if (this.arity == arguments.length) { // EXACT and THUNK rules
        return this.evaluate.apply(this, arguments);
    } else if (this.arity < arguments.length) { // CALLK and TCALL rules
        var remainingArguments = Array.prototype.slice.call(arguments, this.arity, arguments.length);
        arguments.length = this.arity;
        var result = this.evaluate.apply(this, arguments);
        return result.hscall.apply(result, remainingArguments);
    } else if (arguments.length == 0) { // RETFUN
        return this;
    } else if (this instanceof $hs_Pap) { // PCALL rule, we can bypass this rule by building PAPs of PAPs
        return this.evaluate.apply(this, arguments);
    } else {
        // PAP2 rule and then RETFUN (jump to continuation)
        return new $hs_Pap (this, arguments);
    }
};

var $hs_Pap = function(obj, args) {
    this.arity = obj.arity - args.length;
    this.object = obj;
    this.savedArguments = args;
};
$hs_Pap.prototype = {
    hscall: $hs_hscall,
    notEvaluated: false,
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

/**
 * @constructor
 */
function $Func(a, f, info) {
    this.arity = a;
    this.evaluate = f;
    this.n = info;
};
$Func.prototype = {
    hscall: $hs_hscall,
    notEvaluated: false
};
function $F(a, f, info) {
    return new $Func(a, f, info);
};
function $f(a, f, info) {
    return new $Func(a, f, info);
};

/**
 * @constructor
 */
function $Thunk(f, info) {
    this.evaluateOnce=f;
    this.info=info;
};
$Thunk.prototype = {
    hscall: $hs_hscall,
    arity: 0,
    notEvaluated: true,
    evaluate: function() {
        var res = this.evaluateOnce();
        this.evaluate = function () { return res; };
        return res;
    }
};
function $T(f, info) {
    return new $Thunk(f, info);
};
function $t(f, info) {
    return new $Thunk(f, info);
};

/**
 * @constructor
 */
function $Data(t, f, info) {
    this.g = t;
    this.evaluateOnce = f;
    this.info = info;
};
$Data.prototype = {
    hscall: $hs_hscall,
    arity: 0,
    notEvaluated: true,
    evaluate: function() {
        this.v = this.evaluateOnce();
        this.notEvaluated = false;
        var _this = this;
        this.evaluate = function () { return _this; }
        return this;
    }
};
function $D(tag, f, info) {
//    if (typeof f != 'function')
//        throw "Not a function!";
    return new $Data(tag, f, info);
};

/**
 * @constructor
 */
function $DataValue(t, v, info) {
    this.g = t;
    this.v = v;
    this.info = info;
};
$DataValue.prototype = {
    hscall: $hs_hscall,
    arity: 0,
    notEvaluated: false,
    evaluate: function() {
        return this;
    }
};
function $R(tag, v, info) {
//    if (typeof f == 'function')
//        throw "A function!";
    return new $DataValue(tag, v, info);
};
function $d(tag, v, info) {
//    if (typeof f == 'function')
//        throw "A function!";
    return new $DataValue(tag, v, info);
};

var $hs_force = function (a, onComplete, onException) {
    var f = a[0];
    var args = Array.prototype.slice.call(a, 1, a.length);
    try {
        onComplete(f.hscall.apply(f, args));
    }
    catch(e) {
        onException(e);
    }
};


