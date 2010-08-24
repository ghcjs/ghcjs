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

