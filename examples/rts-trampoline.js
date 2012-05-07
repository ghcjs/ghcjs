var $tr = {}

/**
 * @constructor Tail call
 * @param {function(...):!Object}    method Method to call.
 * @param {!Object}                  object Object to call the method on.
 * @param {Array.<Object>}           args   Arguments to pass.
 */
$tr.Jump = function(method, object, args) {
    if(HS_DEBUG) {
        if(typeof method !== 'function')
            throw "Not a function!";
        if(object === undefined)
            throw "Object undefined!";
        for(var i = 0; i !== args.length; i++)
            if(args[i] === undefined)
                throw "Undefined args!"
    }
    this.method = method;
    this.object = object;
    this.args = args;
};
if(HS_WEAKS) {
    $tr.Jump.prototype = {
        mark : function(m) {
            if(this.object.mark !== undefined) this.object.mark(m);
            for (var i = 0; i !== this.args.length; i++) {
                if(this.args[i].mark !== undefined) this.args[i].mark(m);
            }
        }
    };
}

/**
 * @constructor Function call
 * @param {function(...):!Object}    method Method to call.
 * @param {!Object}                  object Object to call the method on.
 * @param {Array.<Object>}           args   Arguments to pass.
 * @param {function(Object):!Object} rest   Continuation to handle result.
 * @param {Array.<Object>}           live   Lists things to keep alive.
 *      We won't finalize these as they will be needed in rest.
 */
$tr.Call = function(method, object, args, rest, live) {
    if(HS_DEBUG) {
        if (typeof method !== 'function')
            throw "Not a function!";
        if(object === undefined)
            throw "Object undefined!";
        for(var i = 0; i !== args.length; i++)
            if(args[i] === undefined)
                throw "Undefined args!"
        if (typeof rest !== 'function')
            throw "Not a function!";
        if(HS_WEAKS) {
            for(var i = 0; i !== live.length; i++)
                if(live[i] === undefined)
                    throw "Undefined live var!"
        }
    }
    this.method = method;
    this.object = object;
    this.args = args;
    this.rest = rest;
    if(HS_WEAKS) this.live = live;
};
if(HS_WEAKS) {
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
}

/**
 * @constructor Try/Catch
 * @param {!Object}                  next    What to do next.
 *      Normal a tail call to the try block.
 * @param {function(Object):!Object} catcher Continuation to handle exceptions
 * @param {Array.<Object>}           live    Lists things to keep alive.
 *      We won't finalize these as they will be needed if we catch an exception
 */
$tr.Catch = function(next, catcher, live) {
    if(HS_DEBUG) {
        if(next === undefined)
            throw "Next undefined!";
        if (typeof catcher !== 'function')
            throw "Not a function!";
        if(HS_WEAKS) {
            for(var i = 0; i !== live.length; i++)
                if(live[i] === undefined)
                    throw "Undefined live var!"
        }
    }
    this.next = next;
    this.catcher = catcher;
    if(HS_WEAKS) this.live = live;
};
if(HS_WEAKS) {
    $tr.Catch.prototype = {
        mark : function(m) {
            if(this.next.mark !== undefined) this.next.mark(m);
            for (var i = 0; i !== this.live.length; i++) {
                if(this.live[i].mark !== undefined) this.live[i].mark(m);
            }
            if(this.catcher.mark !== undefined) this.catcher.mark(m);
        }
    };
}

/**
 * @param {!Object}                  next    What to do next.
 *      Normal a tail call to the try block.
 * @param {function(Object):!Object} catcher Continuation to handle exceptions
 * @param {Array.<Object>}           live    Lists things to keep alive.
 *      We won't finalize these as they will be needed if we catch an exception
 * @return {!Object}
 */
$tr.ctch = function(next, catcher, live) {
    if(HS_WEAKS) {
        $tr.currentResult = new $tr.Catch(next, catcher, live);
    }
    else {
        $tr.currentResult = new $tr.Catch(next, catcher);
    }
};

/**
 * @constructor Suspend the current thread
 * @param {function(Object):!Object} resume Continuation to use when resuming
 * @param {Array.<Object>}           live   Lists things to keep alive.
 *      We won't finalize these as they will be needed when we resume
 */
$tr.Suspend = function(resume, live) {
    this.resume = resume;
    if(HS_WEAKS) this.live = live;
};
if(HS_WEAKS) {
    $tr.Suspend.prototype = {
        mark : function(m) {
            for (var i = 0; i !== this.live.length; i++) {
                if(this.live[i].mark !== undefined) this.live[i].mark(m);
            }
        }
    };
}

/**
 * @param {function(Object):!Object} resume Continuation to use when resuming
 * @param {Array.<Object>}           live   Lists things to keep alive.
 *      We won't finalize these as they will be needed when we resume
 * @return {!Object}
 */
$tr.suspend = function(resume, live) {
    if(HS_WEAKS) {
        $tr.currentResult = new $tr.Suspend(resume, live);
    }
    else {
        $tr.currentResult = new $tr.Suspend(resume, live);
    }
};

/**
 * @constructor
 */
$tr.Yield = function(next) {
    this.next = next;
};
if(HS_WEAKS) {
    $tr.Yield.prototype = {
        mark : function(m) {
            if(this.next.mark !== undefined) this.next.mark(m);
        }
    };
}
$tr.yield = function(next) {
    $tr.currentResult = new $tr.Yield(next);
};

/**
 * @constructor
 */
$tr.Result = function(result) {
    this.value = result;
};
if(HS_WEAKS) {
    $tr.Result.prototype = {
        mark : function(m) {
            if(this.value.mark !== undefined) this.value.mark(m);
        }
    };
}

function $j(method, object, args) {
    $tr.currentResult = new $tr.Jump(method, object, args);
};

function $c(method, object, args, rest, live) {
    if(HS_WEAKS) {
        $tr.currentResult = new $tr.Call(method, object, args, rest, live);
    }
    else {
        $tr.currentResult = new $tr.Call(method, object, args, rest);
    }
};

function $M(object, rest, live) {
    if(object.notEvaluated) {
        if(HS_WEAKS) {
            $tr.currentResult = new $tr.Call(object.hscall, object, [], rest, live);
        }
        else {
            $tr.currentResult = new $tr.Call(object.hscall, object, [], rest);
        }
    }
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
        if(HS_DEBUG && s.running) throw "Already Running";
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
    start : function(r, onComplete, onException) {
        var s = $tr.Scheduler;
        var thread = new $tr.Thread(r, onComplete, onException);
        s.schedule(thread);
        s.maxID++;
        thread.threadID = s.maxID;
        return thread;
    },
    finalizerMark : 0,
    scheduleFinalizers : function() {
        if(HS_WEAKS) {
            var s = $tr.Scheduler;

            // Mark all non top level reachable nodes
            if($tr.Weaks.length === 0)
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
                if($tr.Weaks[i].keepMark !== s.finalizerMark) {
                    // Schedule finalizer and set weak pointer to null
                    s.start($tr.Weaks[i].finalize.hscall($tr.Weaks[i].realWorld));
                    changed = true;

                    // Set these to null so that deRefWeak will return null
                    $tr.Weaks[i].value = null;
                    $tr.Weaks[i].finalize = null;
                    $tr.Weaks[i].realWorld = null;
                }
            }

            // Update the list of weak references if necessary
            if(changed) {
                var newWeaks = [];
                for(i = 0; i !== $tr.Weaks.length; i++) {
                    if($tr.Weaks[i].keepMark === s.finalizerMark) {
                        newWeaks.push($tr.Weaks[i]);
                    }
                }
                $tr.Weaks = newWeaks;
            }
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
$tr.Thread = function (next, onComplete, onException) {
  this.next = next;
  this.isException = false;
  this._stack = [];
  this._stackMax = 1000000;
  this._state = "run";
  this.waitingThreads = [];
  this._onComplete = onComplete;
  this._onException = onException;
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
      return new $tr.Suspend(null, []);
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
        if(HS_WEAKS) $tr.Scheduler.scheduleFinalizers();
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
        if (this._stack.length > this._stackMax) {
            this._stack = [];
            throw "Stack Overflow";
        }

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
            if(this._onException !== undefined) this._onException(r);
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
            if(this._onComplete !== undefined) this._onComplete(r.value);
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
            if(this._onComplete !== undefined) this._onComplete(r);
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
  }
};
if(HS_WEAKS) {
    $tr.Thread.prototype.mark = function(m) {
        if(this.next !== null && this.next.mark !== undefined) this.next.mark(m);
        for (var i = 0; i !== this._stack.length; i++) {
            var live = this._stack[i][2];
            for (var j = 0; j !== live.length; j++)
                if(live[j].mark!==undefined) live[j].mark(m);
        }
   };
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
        return new $tr.Call(this.evaluate, this, arguments,
            function (result) {return new $tr.Jump(result.hscall, result, remainingArguments)},
            remainingArguments);
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
    C : function(args, rest, live) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, rest, live);
    }
};
if(HS_WEAKS) {
    $hs.Pap.prototype.mark = function(m) {
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
function $Func(a, f, live, info) {
    this.arity = a;
    this.evaluate = f;
    if(HS_WEAKS) {
        this.live = live;
        if($hs.loadingModule) this.isTopLevel = true;
    }
    if(HS_DEBUG) {
        this.info = info;
        if (typeof f !== 'function')
            throw "Not a function!";
        if(HS_WEAKS && live !== undefined) {
            for(var i = 0; i !== live.length; i++)
                if(live[i] === undefined)
                    throw "Undefined live var!";
        }
    }
};
function $F(a, f, info) {
    if(HS_DEBUG)  return new $Func(a, f, undefined, info);
    if(!HS_DEBUG) return new $Func(a, f);
};
function $f(a, f, live, info) {
    if(HS_DEBUG && HS_WEAKS)   return new $Func(a, f, live, info);
    if(!HS_DEBUG && HS_WEAKS)  return new $Func(a, f, live);
    if(HS_DEBUG && !HS_WEAKS)  return new $Func(a, f, undefined, info);
    if(!HS_DEBUG && !HS_WEAKS) return new $Func(a, f);
};
function $S(o, live) {
    if(HS_WEAKS) o.live = live;
}
$Func.prototype = {
    hscall: $hs.hscall,
    notEvaluated: false,
    J : function() {
        $tr.currentResult = new $tr.Jump(this.hscall, this, arguments);
    },
    C : function(args, rest, live) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, rest, live);
    }
};
if(HS_WEAKS) {
    $Func.prototype.mark = function(m) {
        if(this.isTopLevel===undefined && this.marked!==m) {
            this.marked = m;
            $tr.markWeaks(this,m);
            for (var i = 0; i !== this.live.length; i++) {
                if(this.live[i].mark !== undefined)
                    this.live[i].mark(m);
                $tr.markWeaks(this.live[i],m);
            }
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
function $Thunk(f, live, info) {
    this.evaluateOnce = f;
    if(HS_WEAKS) {
        this.live = live;
        if($hs.loadingModule) this.isTopLevel = true;
    }
    if(HS_DEBUG) {
        this.info = info;
        if (typeof f !== 'function')
            throw "Not a function!";
        if(HS_WEAKS && live !== undefined) {
            for(var i = 0; i !== live.length; i++)
                if(live[i] === undefined)
                    throw "Undefined live var!";
        }
    }
};
function $T(f, info) {
    if(HS_DEBUG)  return new $Thunk(f, undefined, info);
    if(!HS_DEBUG) return new $Thunk(f);
};
function $t(f, live, info) {
    if(HS_DEBUG && HS_WEAKS)   return new $Thunk(f, live, info);
    if(!HS_DEBUG && HS_WEAKS)  return new $Thunk(f, live);
    if(HS_DEBUG && !HS_WEAKS)  return new $Thunk(f, undefined, info);
    if(!HS_DEBUG && !HS_WEAKS) return new $Thunk(f);
};
$Thunk.prototype = {
    hscall: $hs.hscall,
    arity: 0,
    notEvaluated: true,
    evaluate: function() {
        var _this = this;
        return new $tr.Call(this.evaluateOnce, this, [], function (res) {
            _this.result = res;
            _this.live = [];
            _this.evaluate = function () { return new $tr.Result(_this.result); };
            return new $tr.Result(_this.result);
        }, []);
    },
    J : function() {
        $tr.currentResult = new $tr.Jump(this.hscall, this, arguments);
    },
    C : function(args, rest, live) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, rest, live);
    }
};
if(HS_WEAKS) {
    $Thunk.prototype.mark = function(m) {
        if(this.isTopLevel===undefined && this.marked!==m) {
            this.marked = m;
            $tr.markWeaks(this,m);
            if(this.result!==undefined && this.result.mark!==undefined) {
                this.result.mark(m);
                $tr.markWeaks(this.result,m);
            }
            for (var i = 0; i !== this.live.length; i++) {
                if(this.live[i].mark !== undefined)
                    this.live[i].mark(m);
                $tr.markWeaks(this.live[i],m);
            }
        }
    };
}
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
    if(HS_WEAKS && $hs.loadingModule) this.isTopLevel = true;
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
        return new $tr.Call(_this.evaluateOnce, _this, [], function (res) {
            if (!(res instanceof Array))
                throw "Not an array!";
            for(var n = 0; n !== res.length; n++)
                if(res[n] === undefined)
                    throw "Undefined"
            _this.v = res;
            _this.notEvaluated = false;
            _this.evaluate = function () { return new $tr.Result(_this); };
            return new $tr.Result(_this);
        }, []);
    },
    J : function() {
        $tr.currentResult = new $tr.Jump(this.hscall, this, arguments);
    },
    C : function(args, rest, live) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, rest, live);
    }
};
if(HS_WEAKS) {
    $Data.prototype.mark = function(m) {
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
    if(HS_WEAKS && $hs.loadingModule) this.isTopLevel = true;
    if(HS_DEBUG) this.info = info;
};
function $R(tag, v, info) {
    if(HS_DEBUG) {
        if (!(v instanceof Array))
            throw "Not an array!";
        for(var n = 0; n !== v.length; n++)
            if(v[n] === undefined)
                throw "Undefined";
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
                throw "Undefined";
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
    C : function(args, rest, live) {
        $tr.currentResult = new $tr.Call(this.hscall, this, args, rest, live);
    }
}
if(HS_WEAKS) {
    $DataValue.prototype.mark = function(m) {
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

// Run haskell function
$hs.force = function (args, onComplete, onException) {
    var f = args[0];
    var args = Array.prototype.slice.call(args, 1, args.length);
    $tr.Scheduler.start(f.hscall.apply(f, args), onComplete, onException);
    if(!$tr.Scheduler.running)
        $tr.Scheduler.runWaiting();
};

// Schedule a haskell function to run the next time haskell runs
$hs.schedule = function (args, onComplete, onException) {
    var f = args[0];
    var args = Array.prototype.slice.call(args, 1, args.length);
    $tr.Scheduler.start(f.hscall.apply(f, args), onComplete, onException);
};

// --- Threads ---
$hs.forkzh = function (a, s) {
    var t = $tr.Scheduler.start(a.hscall(s));
    $tr.traceThread("fork thread " + t.threadID);
    return new $tr.Result([s, t]);
};
$hs.forkOnzh = function (n, a, s) {
    return $hs.forkzh(a,s);
};
$hs.yieldzh = function (s) {
    $tr.traceThread("yield thread");
    return new $tr.Yield(new $tr.Result(s));
},
$hs.myThreadIdzh = function (s) {
    return new $tr.Result([s, $tr.Scheduler.currentThread]);
},
$hs.isCurrentThreadBoundzh = function (s) {
    return [s, 1];
};
$hs.noDuplicatezh = function (s) {
    return s;
};

$hs.atomicModifyMutVarzh = function (a, b, s) {
    return new $tr.Call(b.hscall, b, [a.value], function (res) {
            a.value = res.v[0];
            return new $tr.Result([s, res.v[1]]);
        }, [a, s]);
};

// --- Synchronized Mutable Variables ---
/**
 * @constructor MVar
 */
$tr.MVar = function(value, waiting) {
    this.value = null;
    this.waiting = [];
    if($hs.loadingModule) this.isTopLevel = true;
};
if(HS_WEAKS) {
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
}
$hs.newMVarzh = function(s) {
    $tr.traceMVar("newMVar");
    return [s, new $tr.MVar()];
};
$hs.takeMVarzh = function (a, s) {
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
        return new $tr.Suspend(takeWhenNotEmpty, [a,s]);
    }
    return takeWhenNotEmpty(null);
};
$hs.tryTakeMVarzh = function (a, s) {
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
};
$hs.putMVarzh = function (a, b, s) {
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
        return new $tr.Suspend(putWhenEmpty, [a,b,s]);
    }
    return putWhenEmpty(null);
};
$hs.sameMVarzh = function (a, b, s) {
    return [s, a === b];
};
$hs.isEmptyMVarzh = function (a, b, s) {
    return [s, a.value === null];
};

// --- Exceptions ---
$hs.catchzh = function(a, b, s) {
    return new $tr.Catch(
        new $tr.Jump(a.hscall, a, [s]),
        function (e) { return new $tr.Jump(b.hscall, b, [e, s]); },
        [b, s]);
};
$hs.raisezh = function(a) {
    $tr.traceException("raise");
    throw a;
};
$hs.raiseIOzh = function(a, s) {
    $tr.traceException("raiseIO");
    throw a;
};

// TODO add support for async excpetions
//$hs.killThreadzh = function (t, e, s) {
//    $tr.traceThread("kill thread");
//};
$hs.maskAsyncExceptionszh = function (a, s) {
    return new $tr.Jump(a.hscall, a, [s]);
};
$hs.unmaskAsyncExceptionszh = function (a, s) {
    return new $tr.Jump(a.hscall, a, [s]);
};
$hs.getMaskingStatezh = function (s) {
    return new $tr.Result([s, 0]);
};

// --- Weak pointers and finalizers ---
if(HS_WEAKS) {
    $tr.Weaks = [];
}

/**
 * @constructor Week pointer.  Weak pointers do not have a mark
 * function.  Only the key can cause it to be marked with a keepMark
 * (via markWeaks).  This will let us know that the weak pointer is
 * still needed.
 */
$tr.Weak = function(key, value, finalize, realWorld) {
    this.value = value;
    this.finalize = finalize;
    this.realWorld = realWorld;
    if(HS_WEAKS && key.isTopLevel===undefined) {
        if(key.weaks===undefined)
            key.weaks=[];
        key.weaks.push(this);
        $tr.Weaks.push(this);
    }
};
if(HS_WEAKS) {
    $tr.markWeaks = function(o,m) {
        // If the object has weak pointers mark them as still in use
        var ws = o.weaks;
        if(ws !== undefined) {
            for (var w = 0; w !== ws.length; w++) {
                // Different name from a regular mark
                ws[w].keepMark = m;
            }
        }
    };
}
$hs.mkWeakzh = function(o, b, c, s) {
    return [s, new $tr.Weak(o, b, c, s)];
};
$hs.mkWeakForeignEnvzh = function(o, b, w, x, y, z, s) {
    return [s, new $tr.Weak(o, b, c, s)];
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

// --- Parallelism ---
$hs.seqzh = function(a, s) {
    if(a.notEvaluated) {
        if(HS_WEAKS) {
            return new $tr.Call(a.hscall, a, [], function(result) {
                return new $tr.Result([s, result]);
            }, [s]);
        }
        else {
            return new $tr.Call(a.hscall, a, [], function(result) {
                return new $tr.Result([s, result]);
            });
        }
    }
    else
        return new $tr.Result([s, a]);
};

// --- IO ---
$hs.waitReadzh = function(fd, s) {
    var f = $hs.allFiles[fd];
    if(f.text.length > f.fptr)
        return s;
    f.waitingToRead.push($tr.Scheduler.currentThread);
    return new $tr.Suspend(function(_) { return s; }, [s]);
};
$hs.waitWritezh = function(fd, s) {
    return s;
//    var f = $hs.allFiles[fd];
//    f.waitingToWrite.push($tr.Scheduler.currentThread);
//    return new $tr.Suspend(function(_) { return s; }, [s]);
};
