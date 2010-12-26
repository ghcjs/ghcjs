$hs.hscall = function () {
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
};

$hs.Pap = function(obj, args) {
    this.arity = obj.arity - args.length;
    this.object = obj;
    this.savedArguments = args;
};
$hs.Pap.prototype = {
    hscall: $hs.hscall,
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

$hs.Func = function(a) {
    this.arity = a;
};
$hs.Func.prototype = {
    hscall: $hs.hscall,
    notEvaluated: false
};

$hs.Thunk = function() {};
$hs.Thunk.prototype = {
    hscall: $hs.hscall,
    arity: 0,
    notEvaluated: true,
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
    notEvaluated: false,
    evaluate: function() {
        return this;
    }
};

$hs.force = function () {
    var f = arguments[0];
    var args = Array.prototype.slice.call(arguments, 1, arguments.length);
    return f.hscall.apply(f, args);
};

