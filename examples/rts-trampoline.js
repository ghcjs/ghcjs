$trampoline = {
  trcall: function(method, thisArg, args) {
    while ((r = method.apply(thisArg, args)) instanceof $trampoline.Jump) {
      method = r.method;
      thisArg = r.object;
      args = r.args;
    }
    return r.value;
  }
}

$trampoline.Jump = function(method, object, args) {
    this.method = method;
    this.object = object;
    this.args = args;
};

$trampoline.Result = function(result) {
    this.value = result;
};

$hs.hscall = function () {
    if (this.arity == arguments.length) { // EXACT and THUNK rules
        return new $trampoline.Jump(this.evaluate, this, arguments);
    } else if (this.arity < arguments.length) { // CALLK and TCALL rules
        var remainingArguments = Array.prototype.slice.call(arguments, this.arity, arguments.length);
        arguments.length = this.arity;
        var result = $trampoline.trcall(this.evaluate, this, arguments);
        return new $trampoline.Jump(result.hscall, result, remainingArguments);
    } else if (arguments.length == 0) { // RETFUN
        return new $trampoline.Result(this);
    } else if (this instanceof $hs.Pap) { // PCALL rule, we can bypass this rule by building PAPs of PAPs
        return new $trampoline.Jump(this.evaluate, this, arguments);
    } else {
        // PAP2 rule and then RETFUN (jump to continuation)
        return new $trampoline.Result(new $hs.Pap (this, arguments));
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
            return new $trampoline.Jump(this.object.hscall, this.object, newArguments);
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
        var res = $trampoline.trcall(this.evaluateOnce, this, []);
        this.evaluate = function () { return new $trampoline.Result(res); };
        return new $trampoline.Result(res);
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
        return new $trampoline.Result(this);
    }
};

$hs.force = function () {
    var f = arguments[0];
    var args = Array.prototype.slice.call(arguments, 1, arguments.length);
    return $trampoline.trcall(f.hscall, f, args);
};

