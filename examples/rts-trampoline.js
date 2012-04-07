var $tr = {}

/**
 * @constructor Tail call
 * @param {function(...):!Object}    method Method to call.
 * @param {!Object}                  object Object to call the method on.
 * @param {Array.<Object>}           args   Arguments to pass.
 */
$tr.Jump = function(method, object, args) {
    if(HS_DEBUG) {
        if (typeof method !== 'function')
            throw "Not a function!";
    }
    this.method = method;
    this.object = object;
    this.args = args;
};
$tr.Jump.prototype = {
    mark : function(m) {
        if(this.object.mark !== undefined) this.object.mark(m);
        for (var i = 0; i !== this.args.length; i++) {
            if(this.args[i].mark !== undefined) this.args[i].mark(m);
        }
    }
};

/**
 * @constructor Function call
 * @param {function(...):!Object}    method Method to call.
 * @param {!Object}                  object Object to call the method on.
 * @param {Array.<Object>}           args   Arguments to pass.
 * @param {Array.<Object>}           live   Lists things to keep alive.
 *      We won't finalize these as they will be needed in rest.
 * @param {function(Object):!Object} rest   Continuation to handle result.
 */
$tr.Call = function(method, object, args, live, rest) {
    if(HS_DEBUG) {
        if (typeof method !== 'function')
            throw "Not a function!";
        if (typeof rest !== 'function')
            throw "Not a function!";
    }
    this.method = method;
    this.object = object;
    this.args = args;
    this.live = live;
    this.rest = rest;
};
$tr.Call.prototype = {
    mark : function(m) {
        if(this.object.mark !== undefined) this.object.mark(m);
        var i = 0;
        for (; i !== this.args.length; i++) {
            if(this.args[i].mark !== undefined) this.args[i].mark(m);
        }
        for (i = 0; i !== this.live.length; i++) {
            if(this.live[i].mark !== undefined) this.live[i].mark(m);
        }
    }
};

/**
 * @constructor Try/Catch
 * @param {!Object}                  next    What to do next.
 *      Normal a tail call to the try block.
 * @param {Array.<Object>}           live    Lists things to keep alive.
 *      We won't finalize these as they will be needed if we catch an exception
 * @param {function(Object):!Object} catcher Continuation to handle exceptions
 */
$tr.Catch = function(next, live, catcher) {
    this.next = next;
    this.live = live;
    this.catcher = catcher;
};
$tr.Catch.prototype = {
    mark : function(m) {
        if(this.next.mark !== undefined) this.next.mark(m);
        for (var i = 0; i !== this.live.length; i++) {
            if(this.live[i].mark !== undefined) this.live[i].mark(m);
        }
        if(this.catcher.mark !== undefined) this.catcher.mark(m);
    }
};

/**
 * @param {!Object}                  next    What to do next.
 *      Normal a tail call to the try block.
 * @param {Array.<Object>}           live    Lists things to keep alive.
 *      We won't finalize these as they will be needed if we catch an exception
 * @param {function(Object):!Object} catcher Continuation to handle exceptions
 * @return {!Object}
 */
$tr.ctch = function(next, live, catcher) {
    $tr.currentResult = new $tr.Catch(next, live, catcher);
};

/**
 * @constructor Suspend the current thread
 * @param {Array.<Object>}           live   Lists things to keep alive.
 *      We won't finalize these as they will be needed when we resume
 * @param {function(Object):!Object} resume Continuation to use when resuming
 */
$tr.Suspend = function(live, resume) {
    this.live = live;
    this.resume = resume;
};
$tr.Suspend.prototype = {
    mark : function(m) {
        for (var i = 0; i !== this.live.length; i++) {
            if(this.live[i].mark !== undefined) this.live[i].mark(m);
        }
    }
};

/**
 * @param {Array.<Object>}           live   Lists things to keep alive.
 *      We won't finalize these as they will be needed when we resume
 * @param {function(Object):!Object} resume Continuation to use when resuming
 * @return {!Object}
 */
$tr.suspend = function(live, resume) {
    $tr.currentResult = new $tr.Suspend(live, resume);
};

/**
 * @constructor
 */
$tr.Yield = function(next) {
    this.next = next;
};
$tr.Yield.prototype = {
    mark : function(m) {
        if(this.next.mark !== undefined) this.next.mark(m);
    }
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
$tr.Result.prototype = {
    mark : function(m) {
        if(this.value.mark !== undefined) this.value.mark(m);
    }
};

function $j(method, object, args) {
    $tr.currentResult = new $tr.Jump(method, object, args);
};

function $c(method, object, args, live, rest) {
    $tr.currentResult = new $tr.Call(method, object, args, live, rest);
};

function $M(object, live, rest) {
    if(object.notEvaluated)
        $tr.currentResult = new $tr.Call(object.hscall, object, [], live, rest);
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
    running : false,
    finalizers : [],
    runWaiting : function() {
        var s = $tr.Scheduler;
        s.running = true;
        while (s.waiting.length !== 0) {
            while (s.waiting.length !== 0) {
                s.currentThread = s.waiting[0];
                s.waiting = Array.prototype.slice.call(s.waiting, 1, s.waiting.length);
                s.currentThread._run();
            }
            s.scheduleFinalizers();
        }
        s.running = false
    },
    schedule : function(thread) {
        $tr.Scheduler.waiting.push(thread);
    },
    start : function(r) {
        var s = $tr.Scheduler;
        var thread = new $tr.Thread(r);
        s.schedule(thread);
        s.maxID++;
        thread.threadID = s.maxID;
        return thread;
    },
    finalizerMark : 0,
    scheduleFinalizers : function() {
        var s = $tr.Scheduler;

        // Mark all non top level reachable nodes
        if($tr.Weaks.length = 0)
            return;
        s.finalizerMark++;
        s.currentThread.mark(s.finalizerMark);
        var i = 0;
        for(; i !== s.waiting.length; i++) {
            s.waiting[i].mark(s.finalizerMark);
        }

        // Enumerate all the weak pointer
        var changed = false;
        for(i = $tr.Weaks.length; i !== 0;) {
            i--;
            if($tr.Weaks[i] !== finalizerMark) {
                // Schedule finalizer and set weak pointer to null
                s.start($tr.Weaks[i].finalize);
                changed = true;
                $tr.Weaks[i].value = null;
                $tr.Weaks[i].finalize = null;
            }
        }

        // Update the list of weak references if necessary
        if(changed) {
            var newWeaks = [];
            for(i = 0; i !== $tr.Weaks.length; i++) {
                if($tr.Weaks[i] === finalizerMark) {
                    newWeaks.push($tr.Weaks[i]);
                }
            }
            $tr.Weaks = newWeaks;
        }
    }
};

$tr.trace = function(msg) {
    console.log($tr.Scheduler.currentThread.threadID + " : " + msg);
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

$tr.counter = 0;

/**
 * @constructor
 */
$tr.Thread = function (next) {
  this.next = next;
  this.isException = false;
  this._stack = [];
  this._stackMax = 10000;
  this._state = "run";
  this.waitingThreads = [];
}

$tr.Thread.prototype = {

  /* Returns true if the Thread has run to completion. */
  finished: function() {
    return this._state !== "run";
  },

  /* Returns the final return value of the Thread.
   * If the Thread ended with an exception, throws the exception that
   * ended the Thread.
   * If the Thread is not yet complete, throws an exception.
   */
  value: function() {
    if (this._state === "return") {
      return this._value;
    } else if (this._state === "throw") {
      throw this._value;
    } else {
      throw "Thread is not complete";
    }
  },

  /* Suspends a running Thread until this Thread is complete. */
  join: function() {
    if (this._state === "run") {
      this.waitingThreads.push($tr.Scheduler.currentThread);
      return new $tr.Suspend([], null);
    } else {
      return new $tr.Result(this.value());
    }
  },

  popReturnHandler : function () {
    var handler = null;
    while(this._stack.length !== 0 && handler === null) {
      handler = this._stack.pop()[0];
    }
    return handler;
  },

  popCatcher : function () {
    var catcher = null;
    while(this._stack.length !== 0 && catcher === null) {
      catcher = this._stack.pop()[1];
    }
    return catcher;
  },

  load : function(method) {
    console.log(method);
  },

  _run : function() {
    var r = this.next;
    var isException = this.isException;
    var traceLog = [];
    while (true) {
      $tr.currentResult = null;

      // See if it is time to let another thread have a go
      $tr.counter++;
      if($tr.counter === 10000) {
        $tr.counter = 0;
        this.next = r;
        this.isException = isException;
        $tr.Scheduler.scheduleFinalizers();
        $tr.Scheduler.schedule(this);
        return;
      }

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
          if (catcher !== null) {
            r = catcher(r);
            if ($tr.currentResult !== null)
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
          if ($tr.currentResult !== null)
            r = $tr.currentResult;
        }
        else if(r instanceof $tr.Call) {
          if (r.method === undefined) this.load(r.method);
          this._stack.push([r.rest, null, r.live]);
          r = r.method.apply(r.object, r.args);
          if ($tr.currentResult !== null)
            r = $tr.currentResult;
        }
        else if(r instanceof $tr.Result) {
          var handler = this.popReturnHandler();
          if (handler !== null) {
            r = handler(r.value);
            if ($tr.currentResult !== null)
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
          this._stack.push([null, r.catcher, r.live]);
          r = r.next;
        }
        else if(r instanceof $tr.Suspend) {
          if(r.resume !== null)
            this._stack.push([r.resume, null, r.live]);
          this.next = null;
          this.isException = false;
          return;
        }
        else if(r instanceof $tr.Yield) {
          this.next = r.next;
          this.isException = false;
          $tr.Scheduler.schedule(this);
          return;
        }
        // Must be just a plain return value
        else {
          var handler = this.popReturnHandler();
          if (handler !== null) {
            r = handler(r);
            if ($tr.currentResult !== null)
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
    var t;
    for (var i = 0; i < this.waitingThreads.length; i++) {
      t = this.waitingThreads[i];
      t.next = this._value;
      t.isException = this._state === "throw";
      $scheduler.schedule(t);
    }
  },

  mark : function(m) {
    this.next.mark(m);
    for (var i = 0; i !== this._stack.length; i++) {
        var live = this._stack[i][2];
        for (var j = 0; j !== live.length; j++)
            live[j].mark(m);
    }
  }
}

if(HS_TRACE_CALLS) {
    function logCall(obj, args) {
        var msg = 'hscall ' + obj.toString();
        if(HS_TRACE_ARGS) {
            for(var n = 0; n !== args.length; n++) {
                msg = msg + ' (' + args[n].toString() + ')';
            }
        }
        console.log(msg);
    };
}

/**
 * @constructor
 */
$hs.hscall = function () {
    if(HS_TRACE_CALLS) logCall(this, arguments);
    var argc = arguments.length;
    if (this.arity === argc) { // EXACT and THUNK rules
        return new $tr.Jump(this.evaluate, this, arguments);
    } else if (this.arity < argc) { // CALLK and TCALL rules
        var remainingArguments = Array.prototype.slice.call(arguments, this.arity, argc);
        arguments.length = this.arity;
        return new $tr.Call(this.evaluate, this, arguments, remainingArguments,
            function (result) {return new $tr.Jump(result.hscall, result, remainingArguments)});
    } else if (argc === 0) { // RETFUN
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
    if($hs.loadingModule) this.isTopLevel = true;
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
    C : function(args, live, rest) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, live, rest);
    },
    mark : function(m) {
        if(this.isTopLevel===undefined && this.marked!==m) {
            this.marked = m;
            $tr.markWeaks(this,m);
            for (var i = 0; i !== this.savedArguments.length; i++) {
                if(this.savedArguments[i].mark !== undefined)
                    this.savedArguments[i].mark(m);
                $tr.markWeaks(this.savedArguments[i],m);
            }
        }
    }
};

/**
 * @constructor
 */
function $Func(a, f, info) {
    this.arity = a;
    this.evaluate = f;
    if($hs.loadingModule) this.isTopLevel = true;
    if(HS_DEBUG) this.info = info;
};
function $F(a, f, info) {
    if(HS_DEBUG)  return new $Func(a, f, info);
    if(!HS_DEBUG) return new $Func(a, f);
};
function $f(a, f, info) {
    if(HS_DEBUG)  return new $Func(a, f, info);
    if(!HS_DEBUG) return new $Func(a, f);
};
$Func.prototype = {
    hscall: $hs.hscall,
    notEvaluated: false,
    J : function() {
        $tr.currentResult = new $tr.Jump(this.hscall, this, arguments);
    },
    C : function(args, live, rest) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, live, rest);
    },
    mark : function(m) {
        if(this.isTopLevel===undefined && this.marked!==m) {
            this.marked = m;
            $tr.markWeaks(this,m);
        }
    }
};
if(HS_DEBUG) {
    $Func.prototype.toString = function () {
        return this.info + ' arity=' + this.arity;
    };
}

/**
 * @constructor
 */
function $Thunk(f, info) {
    this.evaluateOnce = f;
    if($hs.loadingModule) this.isTopLevel = true;
    if(HS_DEBUG) this.info = info;
};
function $T(f, info) {
    if(HS_DEBUG)  return new $Thunk(f, info);
    if(!HS_DEBUG) return new $Thunk(f);
};
function $t(f, info) {
    if(HS_DEBUG)  return new $Thunk(f, info);
    if(!HS_DEBUG) return new $Thunk(f);
};
$Thunk.prototype = {
    hscall: $hs.hscall,
    arity: 0,
    notEvaluated: true,
    evaluate: function() {
        var _this = this;
        return new $tr.Call(this.evaluateOnce, this, [], [], function (res) {
            _this.result = res;
            _this.evaluate = function () { return new $tr.Result(_this.result); };
            return new $tr.Result(_this.result);
        });
    },
    J : function() {
        $tr.currentResult = new $tr.Jump(this.hscall, this, arguments);
    },
    C : function(args, live, rest) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, live, rest);
    },
    mark : function(m) {
        if(this.isTopLevel===undefined && this.marked!==m) {
            this.marked = m;
            $tr.markWeaks(this,m);
            if(this.result!==undefined&&this.result.mark!==undefined) {
                this.result.mark(m);
                $tr.markWeaks(this.result,m);
            }
        }
    }
};
if(HS_DEBUG) {
    $Thunk.prototype.toString = function () {
        return this.info;
    };
}

/**
 * @constructor
 */
function $Data(t, f, info) {
    this.g = t;
    this.evaluateOnce = f;
    if($hs.loadingModule) this.isTopLevel = true;
    if(HS_DEBUG) this.info = info;
};
function $D(tag, f, info) {
    if(HS_DEBUG) {
        if (typeof f !== 'function')
            throw "Not a function!";
        return new $Data(tag, f, info);
    }
    if(!HS_DEBUG) {
        return new $Data(tag, f);
    }
};
$Data.prototype = {
    hscall: $hs.hscall,
    arity: 0,
    notEvaluated: true,
    evaluate: function() {
        var _this = this;
        return new $tr.Call(_this.evaluateOnce, _this, [], [], function (res) {
            if (!(res instanceof Array))
                throw "Not an array!";
            for(var n = 0; n !== res.length; n++)
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
    C : function(args, live, rest) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, live, rest);
    },
    mark : function(m) {
        if(this.isTopLevel===undefined && this.marked!==m) {
            this.marked = m;
            $tr.markWeaks(this,m);
            if(this.v!==undefined) {
                for (var i = 0; i < this.v.length; i++) {
                    if(this.v[i].mark !== undefined)
                        this.v[i].mark(m);
                    $tr.markWeaks(this.v[i],m);
                }
            }
        }
    }
};
if(HS_DEBUG) {
    $Data.prototype.toString = function () {
        var msg = this.info;
        if(!this.notEvaluated) {
            for(var n = 0; n !== this.v.length; n++) {
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

/**
 * @constructor
 */
function $DataValue(t, v, info) {
    this.g = t;
    this.v = v;
    if($hs.loadingModule) this.isTopLevel = true;
    if(HS_DEBUG) this.info = info;
};
function $R(tag, v, info) {
    if(HS_DEBUG) {
        if (!(v instanceof Array))
            throw "Not an array!";
        for(var n = 0; n !== v.length; n++)
            if(v[n] === undefined)
                throw "Undefined"
        $tr.currentResult = new $DataValue(tag, v, info);
    }
    if(!HS_DEBUG) {
        $tr.currentResult = new $DataValue(tag, v);
    }
};
function $d(tag, v, info) {
    if(HS_DEBUG) {
        if (!(v instanceof Array))
            throw "Not an array!";
        for(var n = 0; n !== v.length; n++)
            if(v[n] === undefined)
                throw "Undefined"
        return new $DataValue(tag, v, info);
    }
    if(!HS_DEBUG) {
        return new $DataValue(tag, v);
    }
};
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
    C : function(args, live, rest) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, live, rest);
    },
    mark : function(m) {
        if(this.isTopLevel===undefined && this.marked!==m) {
            this.marked = m;
            $tr.markWeaks(this,m);
            for (var i = 0; i < this.v.length; i++) {
                if(this.v[i].mark !== undefined)
                    this.v[i].mark(m);
                $tr.markWeaks(this.v[i],m);
            }
        }
    }
};
if(HS_DEBUG) {
    $DataValue.prototype.toString = function () {
        var msg = this.info;
        if(!this.notEvaluated) {
            for(var n = 0; n !== this.v.length; n++) {
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

// Run haskell function and get the result
$hs.force = function () {
    if($tr.Scheduler.running)
        throw "Haskell already running.  Consider using $hs.fork or $hs.schedule."
    var f = arguments[0];
    var args = Array.prototype.slice.call(arguments, 1, arguments.length);
    var t = $tr.Scheduler.start(f.hscall.apply(f, args));
    $tr.Scheduler.runWaiting();
    return t.value();
};

// Run haskell function
$hs.fork = function () {
    var f = arguments[0];
    var args = Array.prototype.slice.call(arguments, 1, arguments.length);
    var t = $tr.Scheduler.start(f.hscall.apply(f, args));
    if(!$tr.Scheduler.running)
        $tr.Scheduler.runWaiting();
};

// Schedule a haskell function to run the next time haskell runs
$hs.schedule = function () {
    var f = arguments[0];
    var args = Array.prototype.slice.call(arguments, 1, arguments.length);
    var t = $tr.Scheduler.start(f.hscall.apply(f, args));
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
        return new $tr.Result([s, $tr.Scheduler.currentThread]);
    },
    noDuplicate : function (s) { return s; }
};

$hs.MutVar.atomicModify = function (a, b, s) {
    return new $tr.Call(b.hscall, b, [a.value], [a, s], function (res) {
        a.value = res.v[0];
        return new $tr.Result([s, res.v[1]]);
      });
};

/**
 * @constructor MVar
 */
$tr.MVar = function(value, waiting) {
    this.value = null;
    this.waiting = [];
    if($hs.loadingModule) this.isTopLevel = true;
};
$tr.MVar.prototype = {
    mark : function(m) {
        if(this.isTopLevel===undefined && this.marked!==m) {
            this.marked = m;
            $tr.markWeaks(this,m);
            if(this.value !== null) {
                if(this.value.mark !== undefined) this.value.mark(m);
                $tr.markWeaks(this.value,m);
            }
            for(var i = 0; i !== this.waiting.length; i++) {
                if(this.waiting[i].mark !== undefined) this.waiting[i].mark(m);
            }
        }
    }
};

$hs.MVar = {
    newMVar : function(s) {
        $tr.traceMVar("newMVar");
        return [s, new $tr.MVar()];
    },
    take : function (a, s) {
        var takeWhenNotEmpty = function (_) {
            $tr.traceMVar("take taking");
            var result = a.value;
            a.value = null;
            if (a.waiting.length !== 0) {
                $tr.traceMVar("take waking waiters");
                var w = a.waiting[0];
                a.waiting = Array.prototype.slice.call(a.waiting, 1, a.waiting.length)
                $tr.Scheduler.schedule(w);
            }
            return new $tr.Result([s, result]);
        };
        if (a.value === null) {
            $tr.traceMVar("take waiting");
            a.waiting.push($tr.Scheduler.currentThread);
            return new $tr.Suspend([a,s],takeWhenNotEmpty);
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
        if (a.waiting.length !== 0) {
            $tr.traceMVar("tryTake waking waiters");
            var w = a.waiting[0];
            a.waiting = Array.prototype.slice.call(a.waiting, 1, a.waiting.length)
            $tr.Scheduler.schedule(w);
        }
        return new $tr.Result([s, 1, result]);
    },
    put : function (a, b, s) {
        var putWhenEmpty = function (_) {
            $tr.traceMVar("put putting");
            a.value = b;
            if (a.waiting.length !== 0) {
                $tr.traceMVar("put waking waiters");
                var w = a.waiting[0];
                a.waiting = Array.prototype.slice.call(a.waiting, 1, a.waiting.length)
                $tr.Scheduler.schedule(w);
            }
            return new $tr.Result(s);
        }
        if (a.value !== null) {
            $tr.traceMVar("put waiting");
            a.waiting.push($tr.Scheduler.currentThread);
            return new $tr.Suspend([a,b,s],putWhenEmpty);
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
            [b,e,s],
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


$tr.Weaks = [];

/**
 * @constructor Week pointer
 */
$tr.Weak = function(key, value, finalize) {
    this.value = value;
    this.finalize = finalize;
    if(key.isTopLevel===undefined) {
        if(key.weaks===undefined)
            key.weaks=[];
        key.weaks.push(this);
        $tr.Weaks.push(this);
    }
};
$tr.markWeaks = function(o,m) {
    var ws = o.weaks;
    if(ws !== undefined) {
        for (var w = 0; w !== ws.length; w++)
            ws[w].marked = m;
    }
};
$hs.mkWeakzh = function(o, b, c, s) {
    return [s, new $tr.Weak(o, b, c)];
};
$hs.mkWeakForeignEnvzh = function(o, b, w, x, y, z, s) {
    return [s, new $tr.Weak(o, b, c)];
};
$hs.deRefWeakzh = function(p, s) {
    return [s, p.value === null ? 0 : 1, p.value];
};
$hs.finalizeWeakzh = function(p, s) {
    return [s, p.finalize === null ? 0 : 1, p.finalize];
};
$hs.touchzh = function(a, s) {
    return s;
};
