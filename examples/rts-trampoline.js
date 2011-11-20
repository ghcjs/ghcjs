var $tr = {}

/**
 * @constructor
 */
$tr.Jump = function(method, object, args) {
    if(HS_DEBUG) {
        if (typeof method != 'function')
            throw "Not a function!";
    }
    this.method = method;
    this.object = object;
    this.args = args;
};

/**
 * @constructor
 */
$tr.Call = function(method, object, args, rest) {
    if(HS_DEBUG) {
        if (typeof method != 'function')
            throw "Not a function!";
    }
    this.method = method;
    this.object = object;
    this.args = args;
    this.rest = rest;
};

/**
 * @constructor
 */
$tr.Catch = function(next, catcher) {
    this.next = next;
    this.catcher = catcher;
};

$tr.ctch = function(next, catcher) {
    $tr.currentResult = new $tr.Catch(next, catcher);
};

/**
 * @constructor
 */
$tr.WithThread = function(withThreadFunc) {
    this.withThread = withThreadFunc;
};

$tr.withThread = function(withThreadFunc) {
    $tr.currentResult = new $tr.WithThread(withThreadFunc);
};

/**
 * @constructor
 */
$tr.Suspend = function(resume) {
    this.resume = resume;
};

$tr.suspend = function(resume) {
    $tr.currentResult = new $tr.Suspend(resume);
};

/**
 * @constructor
 */
$tr.Yield = function(next) {
    this.next = next;
};

$tr.yield = function(next) {
    $tr.currentResult = new $tr.Yield(next);
};

/**
 * @constructor
 */
$tr.Result = function(result) {
    this.value = result;
};

function $j(method, object, args) {
    $tr.currentResult = new $tr.Jump(method, object, args);
};

function $c(method, object, args, rest) {
    $tr.currentResult = new $tr.Call(method, object, args, rest);
};

function $M(object, rest) {
    if(object.notEvaluated)
        $tr.currentResult = new $tr.Call(object.hscall, object, [], rest);
    else
        rest(object);
};

function $A(object) {
    if(object.notEvaluated)
        $tr.currentResult = new $tr.Jump(object.hscall, object, []);
    else
        $tr.currentResult = new $tr.Result(object);
};

function $r(result) {
    $tr.currentResult = new $tr.Result(result);
};

$tr.Scheduler = {
    waiting : [],
    maxID   : 0,
    runWaiting : function() {
        var s = $tr.Scheduler;
        while (s.waiting.length != 0) {
            var t = s.waiting[0];
            s.activeThread = t[0];
            s.waiting = Array.prototype.slice.call(s.waiting, 1, s.waiting.length);
            t[0]._run(t[1], t[2]);
        }
    },
    schedule : function(thread, retval, isException) {
        var s = $tr.Scheduler;
        s.waiting.push([thread, retval, isException]);
    },
    start : function(r) {
        var s = $tr.Scheduler;
        var thread = new $tr.Thread();
        s.schedule(thread, r, false);
        s.maxID++;
        thread.threadID = s.maxID;
        return thread;
    }
};

$tr.trace = function(msg) {
    console.log($tr.Scheduler.activeThread.threadID + " : " + msg);
};

$tr.traceThread = function(msg) {
    $tr.trace(msg);
};

$tr.traceMVar = function(msg) {
    $tr.trace(msg);
};

$tr.traceException = function(msg) {
    $tr.trace(msg);
};

/**
 * @constructor
 */
$tr.Thread = function () {
  this._stack = [];
  this._stackMax = 10000;
  this._state = "run";
  this.waitingThreads = [];
}

$tr.Thread.prototype = {

  /* Returns true if the Thread has run to completion. */
  finished: function() {
    return this._state != "run";
  },

  /* Returns the final return value of the Thread.
   * If the Thread ended with an exception, throws the exception that
   * ended the Thread.
   * If the Thread is not yet complete, throws an exception.
   */
  value: function() {
    if (this._state == "return") {
      return this._value;
    } else if (this._state == "throw") {
      throw this._value;
    } else {
      throw "Thread is not complete";
    }
  },

  /* Suspends a running Thread until this Thread is complete. */
  join: function() {
    if (this._state == "run") {
      var _this = this;
      return new $tr.WithThread(function(currentThread) {
          _this.waitingThreads.push(currentThread);
          return new $tr.Suspend(null);
        });
    } else {
      return new $tr.Result(this.value());
    }
  },

  popReturnHandler : function () {
    var handler = null;
    while(this._stack.length != 0 && handler == null) {
      handler = this._stack.pop()[0];
    }
    return handler;
  },

  popCatcher : function () {
    var catcher = null;
    while(this._stack.length != 0 && catcher == null) {
      catcher = this._stack.pop()[1];
    }
    return catcher;
  },

  load : function(method) {
    console.log(method);
  },

  _run : function(r, isException) {
    var traceLog = [];
    while (true) {
      $tr.currentResult = null;

      if(HS_TRACE) {
          // Handy for debug
          traceLog.push(r);
          if (traceLog.length > 100)
            traceLog = Array.prototype.slice.call(traceLog, 1, traceLog.length);
      }

      try {
        if (this._stack.length > this._stackMax)
            throw "Stack Overflow"

        if (isException) {
          var catcher = this.popCatcher();
          if (catcher != null) {
            r = catcher(r);
            if ($tr.currentResult != null)
              r = $tr.currentResult;
          }
          else {
            this._state = "throw";
            this._value = r;
            this._signal();
            return;
          }
        }
        if (r instanceof $tr.Jump) {
          if (r.method === undefined) this.load(r.method);
          r = r.method.apply(r.object, r.args);
          if ($tr.currentResult != null)
            r = $tr.currentResult;
        }
        else if(r instanceof $tr.Call) {
          if (r.method === undefined) this.load(r.method);
          this._stack.push([r.rest, null]);
          r = r.method.apply(r.object, r.args);
          if ($tr.currentResult != null)
            r = $tr.currentResult;
        }
        else if(r instanceof $tr.Result) {
          var handler = this.popReturnHandler();
          if (handler != null) {
            r = handler(r.value);
            if ($tr.currentResult != null)
              r = $tr.currentResult;
          }
          else {
            this._state = "return";
            this._value = r.value;
            this._signal();
            return;
          }
        }
        else if(r instanceof $tr.Catch) {
          this._stack.push([null, r.catcher]);
          r = r.next;
        }
        else if(r instanceof $tr.WithThread) {
          r = r.withThread(this);
          if ($tr.currentResult != null)
            r = $tr.currentResult;
        }
        else if(r instanceof $tr.Suspend) {
          if(r.resume != null)
            this._stack.push([r.resume, null]);
          return;
        }
        else if(r instanceof $tr.Yield) {
          $tr.Scheduler.schedule(this, r.next, false);
          return;
        }
        // Must be just a plain return value
        else {
          var handler = this.popReturnHandler();
          if (handler != null) {
            r = handler(r);
            if ($tr.currentResult != null)
              r = $tr.currentResult;
          }
          else {
            this._state = "return";
            this._value = r;
            this._signal();
            return;
          }
        }
        isException = false;
      }
      catch(e) {
        isException = true;
        r = e;
      }
    }
  },

  // Notify joining threads that this thread is complete.
  _signal: function() {
    for (var i = 0; i < this.waitingThreads.length; i++) {
      $scheduler.schedule(
        this.waitingThreads[i],
        this._value,
        this._state == "throw");
    }
  }
}

if(HS_TRACE) {
    function logCall(obj, args) {
        var msg = 'hscall ' + obj.toString();
        for(var n = 0; n != args.length; n++) {
            msg + ' (' + args[n].toString() + ')';
        }
        console.log(msg);
    };
}

/**
 * @constructor
 */
$hs.hscall = function () {
    if(HS_TRACE) logCall(this, arguments);
    var argc = arguments.length;
    if (this.arity == argc) { // EXACT and THUNK rules
        return new $tr.Jump(this.evaluate, this, arguments);
    } else if (this.arity < argc) { // CALLK and TCALL rules
        var remainingArguments = Array.prototype.slice.call(arguments, this.arity, argc);
        arguments.length = this.arity;
        return new $tr.Call(this.evaluate, this, arguments,
            function (result) {return new $tr.Jump(result.hscall, result, remainingArguments)});
    } else if (argc == 0) { // RETFUN
        return new $tr.Result(this);
    } else if (this instanceof $hs.Pap) { // PCALL rule, we can bypass this rule by building PAPs of PAPs
        return new $tr.Jump(this.evaluate, this, arguments);
    } else {
        // PAP2 rule and then RETFUN (jump to continuation)
        return new $tr.Result(new $hs.Pap (this, arguments));
    }
};

/**
 * @constructor
 */
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
        return new $tr.Jump(this.object.hscall, this.object, newArguments);
    },
    J : function() {
        $tr.currentResult = new $tr.Jump(this.hscall, this, arguments);
    },
    C : function(args, rest) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, rest);
    }
};

if(HS_DEBUG) {
    /**
     * @constructor
     */
    function $Func(a, f, info) {
        this.arity = a;
        this.evaluate = f;
        this.info = info;
    };
    function $F(a, f, info) {
        return new $Func(a, f, info);
    };
    function $f(a, f, info) {
        return new $Func(a, f, info);
    };
}
else {
    /**
     * @constructor
     */
    function $Func(a, f) {
        this.arity = a;
        this.evaluate = f;
    };
    function $F(a, f) {
        return new $Func(a, f);
    };
    function $f(a, f, info) {
        return new $Func(a, f);
    };
}
$Func.prototype = {
    hscall: $hs.hscall,
    notEvaluated: false,
    J : function() {
        $tr.currentResult = new $tr.Jump(this.hscall, this, arguments);
    },
    C : function(args, rest) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, rest);
    }
};
if(HS_DEBUG) {
    $Func.prototype.toString = function () {
        return this.info + ' arity=' + this.arity;
    };
}

if(HS_DEBUG) {
    /**
     * @constructor
     */
    function $Thunk(f, info) {
        this.evaluateOnce = f;
        this.info = info;
    };
    function $T(f, info) {
        return new $Thunk(f, info);
    };
    function $t(f, info) {
        return new $Thunk(f, info);
    };
}
else {
    /**
     * @constructor
     */
    function $Thunk(f) {
        this.evaluateOnce = f;
    };
    function $T(f, info) {
        return new $Thunk(f, info);
    };
    function $t(f, info) {
        return new $Thunk(f, info);
    };
}

$Thunk.prototype = {
    hscall: $hs.hscall,
    arity: 0,
    notEvaluated: true,
    evaluate: function() {
        var _this = this;
        return new $tr.Call(this.evaluateOnce, this, [], function (res) {
            _this.evaluate = function () { return new $tr.Result(res); };
            return new $tr.Result(res);
        });
    },
    J : function() {
        $tr.currentResult = new $tr.Jump(this.hscall, this, arguments);
    },
    C : function(args, rest) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, rest);
    }
};
if(HS_DEBUG) {
    $Thunk.prototype.toString = function () {
        return this.info;
    };
}

if(HS_DEBUG) {
    /**
     * @constructor
     */
    function $Data(t, f, info) {
        this.g = t;
        this.evaluateOnce = f;
        this.info = info;
    };
    function $D(tag, f, info) {
        if (typeof f != 'function')
            throw "Not a function!";
        return new $Data(tag, f, info);
    };
}
else {
    /**
     * @constructor
     */
    function $Data(t, f) {
        this.g = t;
        this.evaluateOnce = f;
    };
    function $D(tag, f) {
        return new $Data(tag, f);
    };
}
$Data.prototype = {
    hscall: $hs.hscall,
    arity: 0,
    notEvaluated: true,
    evaluate: function() {
        var _this = this;
        return new $tr.Call(_this.evaluateOnce, _this, [], function (res) {
            if (!(res instanceof Array))
                throw "Not an array!";
            for(var n = 0; n != res.length; n++)
                if(res[n] === undefined)
                    throw "Undefined"
            _this.v = res;
            _this.notEvaluated = false;
            _this.evaluate = function () { return new $tr.Result(_this); };
            return new $tr.Result(_this);
        });
    },
    J : function() {
        $tr.currentResult = new $tr.Jump(this.hscall, this, arguments);
    },
    C : function(args, rest) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, rest);
    }
};
if(HS_DEBUG) {
    $Data.prototype.toString = function () {
        var msg = this.info;
        if(!this.notEvaluated) {
            for(var n = 0; n != this.v.length; n++) {
                if(this.v[n] === undefined)
                    msg = msg + "!!";
                else
                    msg = msg + " (" + this.v[n].toString() + ")";
            }
        }
        else {
          msg = msg + " ??";
        }
        return msg;
    };
}

if(HS_DEBUG) {
    /**
     * @constructor
     */
    function $DataValue(t, v, info) {
        this.g = t;
        this.v = v;
        this.info = info;
    };
    function $R(tag, v, info) {
        if (!(v instanceof Array))
            throw "Not an array!";
        for(var n = 0; n != v.length; n++)
            if(v[n] === undefined)
                throw "Undefined"
        $tr.currentResult = new $DataValue(tag, v, info);
    };
    function $d(tag, v, info) {
        if (!(v instanceof Array))
            throw "Not an array!";
        for(var n = 0; n != v.length; n++)
            if(v[n] === undefined)
                throw "Undefined"
        return new $DataValue(tag, v, info);
    };
}
else {
    /**
     * @constructor
     */
    function $DataValue(t, v) {
        this.g = t;
        this.v = v;
    };
    function $R(tag, v) {
        $tr.currentResult = new $DataValue(tag, v);
    };
    function $d(tag, v) {
        return new $DataValue(tag, v);
    };
}
$DataValue.prototype = {
    hscall: $hs.hscall,
    arity: 0,
    notEvaluated: false,
    evaluate: function() {
        return new $tr.Result(this);
    },
    J : function() {
        $tr.currentResult = new $tr.Jump(this.hscall, this, arguments);
    },
    C : function(args, rest) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, rest);
    }
};
if(HS_DEBUG) {
    $DataValue.prototype.toString = function () {
        var msg = this.info;
        if(!this.notEvaluated) {
            for(var n = 0; n != this.v.length; n++) {
                if(this.v[n] === undefined)
                    msg = msg + "!!";
                else if(this.v[n].toString === undefined)
                    msg = msg + " (" + this.v[n] + ")";
                else
                    msg = msg + " (" + this.v[n].toString() + ")";
            }
        }
        return msg;
    };
}

$hs.force = function () {
    var f = arguments[0];
    var args = Array.prototype.slice.call(arguments, 1, arguments.length);
    var t = $tr.Scheduler.start(f.hscall.apply(f, args));
    $tr.Scheduler.runWaiting();
    return t.value();
};

$hs.Thread = {
    fork : function (a, s) {
        var t = $tr.Scheduler.start(a.hscall(s));
        $tr.traceThread("fork thread " + t.threadID);
        return new $tr.Result([s, t]);
    },
    forkOn : function (n, a, s) {return fork(a,s);},
    yieldThread : function (s) {
        $tr.traceThread("yield thread");
        return new $tr.Yield(new $tr.Result(s));
    },
    myThreadId : function (s) {
        return new $tr.WithThread(function (t) {
            return new $tr.Result([s, t]);
          });
    },
    noDuplicate : function (s) { return s; }
};

$hs.MutVar.atomicModify = function (a, b, s) {
    return new $tr.Call(b.hscall, b, [a.value], function (res) {
        a.value = res.v[0];
        return new $tr.Result([s, res.v[1]]);
      });
};

$hs.MVar = {
    newMVar : function(s) {
        $tr.traceMVar("newMVar");
        return [s, {value : null, waiting : []}];
    },
    take : function (a, s) {
        var takeWhenNotEmpty = function (_) {
            $tr.traceMVar("take taking");
            var result = a.value;
            a.value = null;
            if (a.waiting.length != 0) {
                $tr.traceMVar("take waking waiters");
                var w = a.waiting[0];
                a.waiting = Array.prototype.slice.call(a.waiting, 1, a.waiting.length)
                $tr.Scheduler.schedule(w, null);
            }
            return new $tr.Result([s, result]);
        };
        if (a.value === null) {
            $tr.traceMVar("take waiting");
            return new $tr.WithThread(function (t) {
                a.waiting.push(t);
                return new $tr.Suspend(takeWhenNotEmpty);
              });
        }
        return takeWhenNotEmpty(null);
    },
    tryTake : function (a, s) {
        if (a.value === null) {
            $tr.traceMVar("tryTake nothing to take");
            return new $tr.Result([s, 0, null]);
        }
        $tr.traceMVar("tryTake taking");
        var result = a.value;
        a.value = null;
        if (a.waiting.length != 0) {
            $tr.traceMVar("tryTake waking waiters");
            var w = a.waiting[0];
            a.waiting = Array.prototype.slice.call(a.waiting, 1, a.waiting.length)
            $tr.Scheduler.schedule(w, null);
        }
        return new $tr.Result([s, 1, result]);
    },
    put : function (a, b, s) {
        var putWhenEmpty = function (_) {
            $tr.traceMVar("put putting");
            a.value = b;
            if (a.waiting.length != 0) {
                $tr.traceMVar("put waking waiters");
                var w = a.waiting[0];
                a.waiting = Array.prototype.slice.call(a.waiting, 1, a.waiting.length)
                $tr.Scheduler.schedule(w, null);
            }
            return new $tr.Result(s);
        }
        if (a.value != null) {
            $tr.traceMVar("put waiting");
            return new $tr.WithThread(function (t) {
                a.waiting.push(t);
                return new $tr.Suspend(putWhenEmpty);
              });
        }
        return putWhenEmpty(null);
    },
    same : function (a, b, s) {
        return [s, a === b];
    },
    isEmpty : function (a, b, s) {
        return [s, a.value === null];
    }
};

$hs.Exception = {
    tryCatch : function(a, b, s) {
        return new $tr.Catch(
            new $tr.Jump(a.hscall, a, [s]),
            function (e) { return new $tr.Jump(b.hscall, b, [e, s]); } );
    },
    raise : function(a) {
        $tr.traceException("raise");
        throw a;
    },
    raiseIO : function(a, s) {
        $tr.traceException("raiseIO");
        throw a;
    }
};

$hs.maskAsyncExceptions = function (a, s) {
    return new $tr.Jump(a.hscall, a, [s]);
};

$hs.maskAsyncExceptionszh = function (a, s) {
    return new $tr.Jump(a.hscall, a, [s]);
};

$hs.unmaskAsyncExceptions = function (a, s) {
    return new $tr.Jump(a.hscall, a, [s]);
};

$hs.unmaskAsyncExceptionszh = function (a, s) {
    return new $tr.Jump(a.hscall, a, [s]);
};

$hs.getMaskingStatezh = function (s) {
    return new $tr.Result([s, 0]);
};
