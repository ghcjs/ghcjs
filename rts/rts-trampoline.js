// This is used to return a value to the trampoline function
// without using the return keyword
var $tr_currentResult = null;

/**
 * @constructor Tail call
 * @param {function(...):!Object}     method Method to call.
 * @param {?Object}                   object Object to call the method on.
 * @param {!Array.<Object>|Arguments} args   Arguments to pass.
 */
function $tr_Jump(method, object, args) {
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
    $tr_Jump.prototype = {
        getLive : function() {
            return [[this.object],this.args];
        }
    };
}

/**
 * @constructor Function call
 * @param {function(...):!Object}     method Method to call.
 * @param {?Object}                   object Object to call the method on.
 * @param {!Array.<Object>|Arguments} args   Arguments to pass.
 * @param {function(Object):!Object}  rest   Continuation to handle result.
 * @param {Array.<Object>=}           live   Lists things to keep alive.
 *      We won't finalize these as they will be needed in rest.
 */
function $tr_Call(method, object, args, rest, live) {
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
    $tr_Call.prototype = {
        getLive : function() {
            return [[this.object],this.args,this.live];
        }
    };
}

/**
 * @constructor Try/Catch
 * @param {!Object}                  next    What to do next.
 *      Normal a tail call to the try block.
 * @param {function(Object):!Object} catcher Continuation to handle exceptions
 * @param {Array.<Object>=}          live    Lists things to keep alive.
 *      We won't finalize these as they will be needed if we catch an exception
 */
function $tr_Catch(next, catcher, live) {
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
    $tr_Catch.prototype = {
        getLive : function() {
            return [[this.next],this.live];
        }
    };
}

/**
 * @param {!Object}                  next    What to do next.
 *      Normal a tail call to the try block.
 * @param {function(Object):!Object} catcher Continuation to handle exceptions
 * @param {Array.<Object>=}          live    Lists things to keep alive.
 *      We won't finalize these as they will be needed if we catch an exception
 */
function $tr_ctch(next, catcher, live) {
    if(HS_WEAKS) {
        $tr_currentResult = new $tr_Catch(next, catcher, live);
    }
    else {
        $tr_currentResult = new $tr_Catch(next, catcher);
    }
};

/**
 * @constructor Suspend the current thread
 * @param {?function(Object):!Object} resume Continuation to use when resuming
 * @param {Array.<Object>=}           live   Lists things to keep alive.
 *      We won't finalize these as they will be needed when we resume
 */
function $tr_Suspend(resume, live) {
    this.resume = resume;
    if(HS_WEAKS) this.live = live;
};
if(HS_WEAKS) {
    $tr_Suspend.prototype = {
        getLive : function() {
            return [this.live];
        }
    };
}

/**
 * @param {function(Object):!Object} resume Continuation to use when resuming
 * @param {Array.<Object>=}           live   Lists things to keep alive.
 *      We won't finalize these as they will be needed when we resume
 */
function $tr_suspend(resume, live) {
    if(HS_WEAKS) {
        $tr_currentResult = new $tr_Suspend(resume, live);
    }
    else {
        $tr_currentResult = new $tr_Suspend(resume, live);
    }
};

/**
 * @constructor
 * @param {Object} next
 */
function $tr_Yield(next) {
    this.next = next;
};
if(HS_WEAKS) {
    $tr_Yield.prototype = {
        getLive : function() {
            return [[this.next]];
        }
    };
}
function $tr_yield(next) {
    $tr_currentResult = new $tr_Yield(next);
};

/**
 * @constructor
 * @param {Object} result
 */
function $tr_Result(result) {
    this.value = result;
};
if(HS_WEAKS) {
    $tr_Result.prototype = {
        getLive : function() {
            return [[this.value]];
        }
    };
}

function $j(method, object, args) {
    $tr_currentResult = new $tr_Jump(method, object, args);
};

function $k(method, args) {
    $tr_currentResult = new $tr_Jump(method, null, args);
};

function $c(method, object, args, rest, live) {
    if(HS_WEAKS) {
        $tr_currentResult = new $tr_Call(method, object, args, rest, live);
    }
    else {
        $tr_currentResult = new $tr_Call(method, object, args, rest);
    }
};

function $b(method, args, rest, live) {
    if(HS_WEAKS) {
        $tr_currentResult = new $tr_Call(method, null, args, rest, live);
    }
    else {
        $tr_currentResult = new $tr_Call(method, null, args, rest);
    }
};

function $M(object, rest, live) {
    if(object.notEvaluated) {
        if(HS_WEAKS) {
            $tr_currentResult = new $tr_Call(object.hscall, object, [], rest, live);
        }
        else {
            $tr_currentResult = new $tr_Call(object.hscall, object, [], rest);
        }
    }
    else
        rest(object);
};

function $A(object) {
    if(object.notEvaluated)
        $tr_currentResult = new $tr_Jump(object.hscall, object, []);
    else
        $tr_currentResult = new $tr_Result(object);
};

function $r(result) {
    $tr_currentResult = new $tr_Result(result);
};

var $tr_Scheduler = {
    waiting : [],
    waitingForBundle : [],
    maxID   : 0,
    running : false,
    finalizers : [],
    cpuSeconds : 0,
    currentRunStart : 0,
    runWaiting : function() {
        var s = $tr_Scheduler;
        if(HS_DEBUG && s.running) throw "Already Running";
        s.currentRunStart = new Date().getTime();
        s.running = true;
        while (s.waiting.length !== 0) {
          s.currentThread = s.waiting[0];
          s.waiting = Array.prototype.slice.call(s.waiting, 1, s.waiting.length);
          s.currentThread._run();
          if(new Date().getTime() - s.currentRunStart > 50)
            break;
        }
        s.running = false
        s.cpuSeconds += new Date().getTime() - s.currentRunStart;
        if(s.waiting.length !== 0) {
          setTimeout(function() {s.runWaiting();}, 0);
          return;
        }
        s.scheduleFinalizers();
        if(s.waiting.length !== 0)
          setTimeout(function() {s.runWaiting();}, 0);
    },
    schedule : function(thread) {
        $tr_Scheduler.waiting.push(thread);
    },
    /**
     * @param {!Object}            r
     * @param {function(Object)=}  onComplete
     * @param {function(!Object)=} onException
     */
    start : function(r, onComplete, onException) {
        var s = $tr_Scheduler;
        var thread = new $tr_Thread(r, onComplete, onException);
        s.schedule(thread);
        s.maxID++;
        thread.threadID = s.maxID;
        return thread;
    },
    finalizerMark : 0,
    scheduleFinalizers : function() {
        if(HS_WEAKS) {
            var s = $tr_Scheduler;

            // No weak pointers? then there is nothing to do.
            if($tr_Weaks.length === 0)
                return;

            // Mark all non top level reachable nodes
            s.mark();

            // Enumerate all the weak pointers
            var changed = false;
            for(var i = $tr_Weaks.length; i !== 0;) {
                i--;
                if($tr_Weaks[i].keepMark !== s.finalizerMark) {
                    // Schedule finalizer and set weak pointer to null
                    s.start($tr_Weaks[i].finalize.hscall($tr_Weaks[i].realWorld));
                    changed = true;

                    // Set these to null so that deRefWeak will return null
                    $tr_Weaks[i].value = null;
                    $tr_Weaks[i].finalize = null;
                    $tr_Weaks[i].realWorld = null;
                }
            }

            // Update the list of weak references if necessary
            if(changed) {
                var newWeaks = [];
                for(var i = 0; i !== $tr_Weaks.length; i++) {
                    if($tr_Weaks[i].keepMark === s.finalizerMark) {
                        newWeaks.push($tr_Weaks[i]);
                    }
                }
                $tr_Weaks = newWeaks;
            }
        }
    },
    getLive : function (n) {
        var s = $tr_Scheduler;
        var result = [[s.currentThread],s.waiting];
        for(var i = 0; i != s.waitingForBundle.length; i++)
            if(s.waitingForBundle[i] !== undefined && s.waitingForBundle[i].length !== 0)
                result.push(s.waitingForBundle[i]);
        return result;
    },
    mark : function() {
        var s = $tr_Scheduler;

        // Move on to the next mark value
        s.finalizerMark++;
        var m = s.finalizerMark;

        // To avoid oveflowing the JavaScript stack we have our own
        var workStack = [[0, [s]]];
        var top;
        var item;
        while (workStack.length != 0) {
            top = workStack[workStack.length-1];
            // Something left to do on the top
            if(top[0] != top[1].length) {
                item = top[1][top[0]++];
                if(        item            !== null
                        && item            !== undefined
                        && item.isTopLevel === undefined
                        && item.marked     !== m) {
                    // Mark this item as done
                    item.marked=m;

                    // Add the "keepMark" to an week pointers
                    // that are pointing to this item
                    $tr_markWeaks(item, m);

                    // Add and references this item wants to keep alive
                    // to the stack of work we have.
                    if(item.getLive !== undefined) {
                        var newLive = item.getLive();
                        for(var nlive = 0; nlive !== newLive.length; nlive++)
                            workStack.push([0, newLive[nlive]]);
                    }
                }
            }
            else {
                workStack.pop();
            }
        }
    }
};

function $tr_trace(msg) {
    HS_TRACE && $hs_logger.info($tr_Scheduler.currentThread.threadID + " : " + msg);
};

function $tr_traceThread(msg) {
    $tr_trace(msg);
};

function $tr_traceMVar(msg) {
    $tr_trace(msg);
};

function $tr_traceException(msg) {
    $tr_trace(msg);
};

var $tr_counter = 0;

/**
 * @constructor
 * @param {!Object}            next
 * @param {function(Object)=}  onComplete
 * @param {function(!Object)=} onException
 */
function $tr_Thread(next, onComplete, onException) {
  this.next = next;
  this.isException = false;
  this._stack = [];
  this._stackMax = 1000000;
  this._state = "run";
  this.waitingThreads = [];
  this._onComplete = onComplete;
  this._onException = onException;
}

$tr_Thread.prototype = {

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
      this.waitingThreads.push($tr_Scheduler.currentThread);
      return new $tr_Suspend(null, []);
    } else {
      return new $tr_Result(this.value());
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

  _run : function() {
    var r = this.next;
    var isException = this.isException;
    var traceLog = [];
    var tickLimit = 10000;
    var limit = tickLimit;
    while (true) {
      $tr_currentResult = null;

      // See if it is time to let another thread have a go
      limit--;
      if(limit === 0) {
        this.next = r;
        this.isException = isException;
        if(HS_WEAKS) $tr_Scheduler.scheduleFinalizers();
        $tr_Scheduler.schedule(this);
        break;
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
            if ($tr_currentResult !== null)
              r = $tr_currentResult;
          }
          else {
            this._state = "throw";
            this._value = r;
            this._signal();
            if(this._onException !== undefined) this._onException(r);
            break;
          }
        }
        if (r instanceof $tr_Jump) {
          r = r.method.apply(r.object, r.args);
          if ($tr_currentResult !== null)
            r = $tr_currentResult;
        }
        else if(r instanceof $tr_Call) {
          this._stack.push([r.rest, null, r.live]);
          r = r.method.apply(r.object, r.args);
          if ($tr_currentResult !== null)
            r = $tr_currentResult;
        }
        else if(r instanceof $tr_Result) {
          var handler = this.popReturnHandler();
          if (handler !== null) {
            r = handler(r.value);
            if ($tr_currentResult !== null)
              r = $tr_currentResult;
          }
          else {
            this._state = "return";
            this._value = r.value;
            this._signal();
            if(this._onComplete !== undefined) this._onComplete(r.value);
            break;
          }
        }
        else if(r instanceof $tr_Catch) {
          this._stack.push([null, r.catcher, r.live]);
          r = r.next;
        }
        else if(r instanceof $tr_Suspend) {
          if(r.resume !== null)
            this._stack.push([r.resume, null, r.live]);
          this.next = null;
          this.isException = false;
          break;
        }
        else if(r instanceof $tr_Yield) {
          this.next = r.next;
          this.isException = false;
          $tr_Scheduler.schedule(this);
          break;
        }
        // Must be just a plain return value
        else {
          var handler = this.popReturnHandler();
          if (handler !== null) {
            r = handler(r);
            if ($tr_currentResult !== null)
              r = $tr_currentResult;
          }
          else {
            this._state = "return";
            this._value = r;
            this._signal();
            if(this._onComplete !== undefined) this._onComplete(r);
            break;
          }
        }
        isException = false;
      }
      catch(e) {
        isException = true;
        r = e;
      }
    }
    $tr_counter += (tickLimit-limit);
  },

  // Notify joining threads that this thread is complete.
  _signal: function() {
    var t;
    for (var i = 0; i < this.waitingThreads.length; i++) {
      t = this.waitingThreads[i];
      t.next = this._value;
      t.isException = this._state === "throw";
      $tr_Scheduler.schedule(t);
    }
  }
};
if(HS_WEAKS) {
    $tr_Thread.prototype.getLive = function() {
        var stackLive = [];
        for (var i = 0; i !== this._stack.length; i++) {
            stackLive[i] = this._stack[i][2];
        }
        stackLive[i] = [this.next];
        return stackLive;
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
        $hs_logger.info(msg);
    };
}

/**
 * @this {$hs_Pap|$Func|$Thunk|$Data|$DataValue}
 * @param {...Object} var_args
 */
function $hs_hscall(var_args) {
    if(HS_TRACE_CALLS) logCall(this, arguments);
    var argc = arguments.length;
    if (this.arity === argc) { // EXACT and THUNK rules
        return new $tr_Jump(this.evaluate, this, arguments);
    } else if (this.arity < argc) { // CALLK and TCALL rules
        var remainingArguments = Array.prototype.slice.call(arguments, this.arity, argc);
        arguments.length = this.arity;
        return new $tr_Call(this.evaluate, this, arguments,
            function (result) {return new $tr_Jump(result.hscall, result, remainingArguments)},
            remainingArguments);
    } else if (argc === 0) { // RETFUN
        return new $tr_Result(this);
    } else if (this instanceof $hs_Pap) { // PCALL rule, we can bypass this rule by building PAPs of PAPs
        return new $tr_Jump(this.evaluate, this, arguments);
    } else {
        // PAP2 rule and then RETFUN (jump to continuation)
        return new $tr_Result(new $hs_Pap (this, arguments));
    }
};

/**
 * @constructor
 * @param {!Object}                   obj
 * @param {!Array.<Object>|Arguments} args
 */
function $hs_Pap(obj, args) {
    this.arity = obj.arity - args.length;
    this.object = obj;
    this.savedArguments = args;
    if($hs_loading) this.isTopLevel = true;
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
        return new $tr_Jump(this.object.hscall, this.object, newArguments);
    },
    J : function() {
        $tr_currentResult = new $tr_Jump(this.hscall, this, arguments);
    },
    C : function(args, rest, live) {
        $tr_currentResult = new $tr_Call(this.hscall, this, args, rest, live);
    }
};
if(HS_WEAKS) {
    $hs_Pap.prototype.getLive = function() {
        return [this.savedArguments];
    };
};

/**
 * @constructor
 * @param {number}          a      Arity
 * @param {function(...)}   f      Function definition.
 * @param {Array.<Object>=} live   Lists things to keep alive.
 * @param {string=}         info   Debug info.
 */
function $Func(a, f, live, info) {
    this.arity = a;
    this.evaluate = f;
    if(HS_WEAKS) {
        this.live = live;
        if($hs_loading) this.isTopLevel = true;
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
    hscall: $hs_hscall,
    notEvaluated: false,
    J : function() {
        $tr_currentResult = new $tr_Jump(this.hscall, this, arguments);
    },
    C : function(args, rest, live) {
        $tr_currentResult = new $tr_Call(this.hscall, this, args, rest, live);
    }
};
if(HS_WEAKS) {
    $Func.prototype.getLive = function() {
        return [this.live];
    };
};
if(HS_DEBUG) {
    $Func.prototype.toString = function () {
        return this.info + ' arity=' + this.arity;
    };
}

/**
 * @constructor
 * @param {function(...)}   f      Function definition.
 * @param {Array.<Object>=} live   Lists things to keep alive.
 * @param {string=}         info   Debug info.
 */
function $Thunk(f, live, info) {
    this.evaluateOnce = f;
    if(HS_WEAKS) {
        this.live = live;
        if($hs_loading) this.isTopLevel = true;
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
    hscall: $hs_hscall,
    arity: 0,
    notEvaluated: true,
    evaluate: function() {
        var _this = this;
        return new $tr_Call(this.evaluateOnce, this, [], function (res) {
            _this.result = res;
            _this.live = [];
            _this.evaluate = function () { return new $tr_Result(_this.result); };
            return new $tr_Result(_this.result);
        }, []);
    },
    J : function() {
        $tr_currentResult = new $tr_Jump(this.hscall, this, arguments);
    },
    C : function(args, rest, live) {
        $tr_currentResult = new $tr_Call(this.hscall, this, args, rest, live);
    }
};
if(HS_WEAKS) {
    $Thunk.prototype.getLive = function() {
        return [this.live,[this.result]];
    };
}
if(HS_DEBUG) {
    $Thunk.prototype.toString = function () {
        return this.info;
    };
}

/**
 * @constructor
 * @param {!number}       t      Data tag.
 * @param {function(...)} f      Function that to initialise the data.
 * @param {string=}       info   Debug info.
 */
function $Data(t, f, info) {
    this.g = t;
    this.evaluateOnce = f;
    if(HS_WEAKS && $hs_loading) this.isTopLevel = true;
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
    hscall: $hs_hscall,
    arity: 0,
    notEvaluated: true,
    evaluate: function() {
        var _this = this;
        return new $tr_Call(_this.evaluateOnce, _this, [], function (res) {
            if (!(res instanceof Array))
                throw "Not an array!";
            for(var n = 0; n !== res.length; n++)
                if(res[n] === undefined)
                    throw "Undefined"
            _this.v = res;
            _this.notEvaluated = false;
            _this.evaluate = function () { return new $tr_Result(_this); };
            return new $tr_Result(_this);
        }, []);
    },
    J : function() {
        $tr_currentResult = new $tr_Jump(this.hscall, this, arguments);
    },
    C : function(args, rest, live) {
        $tr_currentResult = new $tr_Call(this.hscall, this, args, rest, live);
    }
};
if(HS_WEAKS) {
    $Data.prototype.getLive = function() {
        return this.v !== undefined ? [this.v] : [];
    };
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
 * @param {!number}         t      Data tag.
 * @param {!Array.<Object>} v      Data values.
 * @param {string=}         info   Debug info.
 */
function $DataValue(t, v, info) {
    this.g = t;
    this.v = v;
    if(HS_WEAKS && $hs_loading) this.isTopLevel = true;
    if(HS_DEBUG) this.info = info;
};
function $R(tag, v, info) {
    if(HS_DEBUG) {
        if (!(v instanceof Array))
            throw "Not an array!";
        for(var n = 0; n !== v.length; n++)
            if(v[n] === undefined)
                throw "Undefined";
        $tr_currentResult = new $DataValue(tag, v, info);
    }
    if(!HS_DEBUG) {
        $tr_currentResult = new $DataValue(tag, v);
    }
};
/**
 * @constructor
 * @param {!number}         tag    Data tag.
 * @param {!Array.<Object>} v      Data values.
 * @param {string=}         info   Debug info.
 * @return {!$DataValue}
 */
function $d(tag, v, info) {
    if(HS_DEBUG) {
        if (!(v instanceof Array))
            throw "Not an array!";
        for(var n = 0; n !== v.length; n++)
            if(v[n] === undefined)
                throw "Undefined";
        return new $DataValue(tag, v, info);
    }
    return new $DataValue(tag, v);
};
$DataValue.prototype = {
    hscall: $hs_hscall,
    arity: 0,
    notEvaluated: false,
    evaluate: function() {
        return new $tr_Result(this);
    },
    J : function() {
        $tr_currentResult = new $tr_Jump(this.hscall, this, arguments);
    },
    C : function(args, rest, live) {
        $tr_currentResult = new $tr_Call(this.hscall, this, args, rest, live);
    }
}
if(HS_WEAKS) {
    $DataValue.prototype.getLive = function() {
        return [this.v];
    };
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
function $hs_force(args, onComplete, onException) {
    var f = args[0];
    var a = Array.prototype.slice.call(args, 1, args.length);
    $tr_Scheduler.start(new $tr_Jump(f.hscall, f, a), onComplete, onException);
    if(!$tr_Scheduler.running)
        $tr_Scheduler.runWaiting();
};

// Schedule a haskell function to run the next time haskell runs
function $hs_schedule(args, onComplete, onException) {
    var f = args[0];
    var a = Array.prototype.slice.call(args, 1, args.length);
    $tr_Scheduler.start(new $tr_Jump(f.hscall, f, a), onComplete, onException);
};

/**
 * @constructor
 */
function $Loader(bundles, f) {
    this.bundles = bundles;
    this.get = f;
};
function $L(bundles, f) {
    return new $Loader(bundles, f);
};
$Loader.prototype = {
    hscall: function() {
        var this_ = this;
        var args = arguments;
        return hs_loadBundles(this.bundles, function() {
            var newFunction = this_.get();
            return newFunction.hscall.apply(newFunction, args);
        });
    },
    // arity: undefined,
    notEvaluated: true,
    evaluate: function() {
        var this_ = this;
        return hs_loadBundles(this.bundles, function() {
            var newFunction = this_.get();
            return newFunction.evaluate();
        });
    },
    J : function() {
        $tr_currentResult = new $tr_Jump(this.hscall, this, arguments);
    },
    C : function(args, rest, live) {
        $tr_currentResult = new $tr_Call(this.hscall, this, args, rest, live);
    }
};
if(HS_WEAKS) {
    $Loader.prototype.getLive = function() {
        return [];
    };
}
function hs_loadBundles(bundles, f) {
    if(bundles.length===0)
        return f();

    var bundle = bundles.pop();
    if($hs_loaded[bundle]===undefined) {
        // Let's load the function set
        if($tr_Scheduler.waitingForBundle[bundle]===undefined) {
            // No one else has asked for this function set lets send a request
            $tr_Scheduler.waitingForBundle[bundle]=[$tr_Scheduler.currentThread];

            var path = $hs_loadPath + "hs" + bundle + (COMPILED ? "min.js" : ".js");

            var transport = new XMLHttpRequest();

            // Set up function to handle the response
            transport.onreadystatechange = function() {
                if(transport.readyState === 4) {
                    var waiting = $tr_Scheduler.waitingForBundle[bundle];
                    try {
                        // Evaluate the response
                        $hs_loading=true;
                        goog.globalEval(transport.responseText
                            + "\n//@ sourceURL="+path);
                        $hs_loading=false;
                        $hs_loaded[bundle] = true; // Don't need to load this again

                        // Wake all the threads waiting for this
                        for(var w = 0; w !== waiting.length; w++) {
                            $tr_Scheduler.schedule(waiting[w]);
                        }
                    } catch (e) {
                        $hs_logError("Error evaluating function set: " + path + ":\n" + e);
                        for(var w = 0; w !== waiting.length; w++) {
                            waiting[w].next = e;
                            waiting[w].isException = true;
                            $tr_Scheduler.schedule(waiting[w]);
                        }
                    }
                    $tr_Scheduler.waitingForBundle[bundle] = [];
                    if(!$tr_Scheduler.running)
                        $tr_Scheduler.runWaiting();
                }
            };

            // Send the request
            transport.open("GET", path, false);
            transport.send(null);
        }
        else {
            // Add this thread to the list of those waiting for the function set
            $tr_Scheduler.waitingForBundle[bundle].push($tr_Scheduler.currentThread);
        }
        return new $tr_Suspend(function(_) {
            return hs_loadBundles(bundles, f);
        }, []);
    }
    else {
        // This one is loaded already
        return hs_loadBundles(bundles, f);
    }
};

// --- Threads ---
function $hs_forkzh(a, s) {
    var t = $tr_Scheduler.start(a.hscall(s));
    $tr_traceThread("fork thread " + t.threadID);
    return [s, t];
};
function $hs_forkOnzh(n, a, s) {
    return $hs_forkzh(a,s);
};
function $hs_yieldzh(s) {
    $tr_traceThread("yield thread");
    return new $tr_Yield(new $tr_Result(s));
};
function $hs_myThreadIdzh(s) {
    return [s, $tr_Scheduler.currentThread];
};
function $hs_isCurrentThreadBoundzh(s) {
    return [s, 1];
};
function $hs_noDuplicatezh(s) {
    return s;
};

function $hs_atomicModifyMutVarzh(a, b, s) {
    return new $tr_Call(b.hscall, b, [a.value], function (res) {
            a.value = res.v[0];
            return new $tr_Result([s, res.v[1]]);
        }, [a, s]);
};

// --- Synchronized Mutable Variables ---
/**
 * @constructor MVar
 */
function $tr_MVar() {
    this.value = null;
    this.waiting = [];
    if($hs_loading) this.isTopLevel = true;
};
if(HS_WEAKS) {
    $tr_MVar.prototype = {
        getLive : function() {
            return [this.waiting,[this.value]];
        }
    };
}
function $hs_newMVarzh(s) {
    $tr_traceMVar("newMVar");
    return [s, new $tr_MVar()];
};
function $hs_takeMVarzh(a, s) {
    var takeWhenNotEmpty = function (_) {
        $tr_traceMVar("take taking");
        var result = a.value;
        a.value = null;
        if (a.waiting.length !== 0) {
            $tr_traceMVar("take waking waiters");
            var w = a.waiting[0];
            a.waiting = Array.prototype.slice.call(a.waiting, 1, a.waiting.length)
            $tr_Scheduler.schedule(w);
        }
        return new $tr_Result([s, result]);
    };
    if (a.value === null) {
        $tr_traceMVar("take waiting");
        a.waiting.push($tr_Scheduler.currentThread);
        return new $tr_Suspend(takeWhenNotEmpty, [a,s]);
    }
    return takeWhenNotEmpty(null);
};
function $hs_tryTakeMVarzh(a, s) {
    if (a.value === null) {
        $tr_traceMVar("tryTake nothing to take");
        return new $tr_Result([s, 0, null]);
    }
    $tr_traceMVar("tryTake taking");
    var result = a.value;
    a.value = null;
    if (a.waiting.length !== 0) {
        $tr_traceMVar("tryTake waking waiters");
        var w = a.waiting[0];
        a.waiting = Array.prototype.slice.call(a.waiting, 1, a.waiting.length)
        $tr_Scheduler.schedule(w);
    }
    return new $tr_Result([s, 1, result]);
};
function $hs_putMVarzh(a, b, s) {
    var putWhenEmpty = function (_) {
        $tr_traceMVar("put putting");
        a.value = b;
        if (a.waiting.length !== 0) {
            $tr_traceMVar("put waking waiters");
            var w = a.waiting[0];
            a.waiting = Array.prototype.slice.call(a.waiting, 1, a.waiting.length)
            $tr_Scheduler.schedule(w);
        }
        return new $tr_Result(s);
    }
    if (a.value !== null) {
        $tr_traceMVar("put waiting");
        a.waiting.push($tr_Scheduler.currentThread);
        return new $tr_Suspend(putWhenEmpty, [a,b,s]);
    }
    return putWhenEmpty(null);
};
function $hs_sameMVarzh(a, b, s) {
    return [s, a === b];
};
function $hs_isEmptyMVarzh(a, b, s) {
    return [s, a.value === null];
};

// --- Exceptions ---
function $hs_catchzh(a, b, s) {
    return new $tr_Catch(
        new $tr_Jump(a.hscall, a, [s]),
        function (e) { return new $tr_Jump(b.hscall, b, [e, s]); },
        [b, s]);
};
function $hs_raisezh(a) {
    $tr_traceException("raise");
    throw a;
};
function $hs_raiseIOzh(a, s) {
    $tr_traceException("raiseIO");
    throw a;
};

// TODO add support for async excpetions
//$hs_killThreadzh = function (t, e, s) {
//    $tr_traceThread("kill thread");
//};
function $hs_maskAsyncExceptionszh(a, s) {
    return new $tr_Jump(a.hscall, a, [s]);
};
function $hs_unmaskAsyncExceptionszh(a, s) {
    return new $tr_Jump(a.hscall, a, [s]);
};
function $hs_getMaskingStatezh(s) {
    return [s, 0];
};

// --- Weak pointers and finalizers ---
if(HS_WEAKS) {
    var $tr_Weaks = [];
}

/**
 * @constructor Week pointer.  Weak pointers do not have a mark
 * function.  Only the key can cause it to be marked with a keepMark
 * (via markWeaks).  This will let us know that the weak pointer is
 * still needed.
 */
function $tr_Weak(key, value, finalize, realWorld) {
    this.value = value;
    this.finalize = finalize;
    this.realWorld = realWorld;
    if(HS_WEAKS && key.isTopLevel===undefined) {
        if(key.weaks===undefined)
            key.weaks=[];
        key.weaks.push(this);
        $tr_Weaks.push(this);
    }
};
if(HS_WEAKS) {
    function $tr_markWeaks(o,m) {
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
function $hs_mkWeakzh(o, b, c, s) {
    return [s, new $tr_Weak(o, b, c, s)];
};
function $hs_mkWeakForeignEnvzh(o, b, w, x, y, z, s) {
    HS_WEAKS && $hs_logger.warning("Weak Foreign Ignored");
    // return [s, new $tr_Weak(o, b, c, s)];
};
function $hs_deRefWeakzh(p, s) {
    return [s, p.value === null ? 0 : 1, p.value];
};
function $hs_finalizeWeakzh(p, s) {
    return [s, p.finalize === null ? 0 : 1, p.finalize];
};
function $hs_touchzh(a, s) {
    return s;
};

// --- Parallelism ---
function $hs_seqzh(a, s) {
    if(a.notEvaluated) {
        if(HS_WEAKS) {
            return new $tr_Call(a.hscall, a, [], function(result) {
                return new $tr_Result([s, result]);
            }, [s]);
        }
        else {
            return new $tr_Call(a.hscall, a, [], function(result) {
                return new $tr_Result([s, result]);
            });
        }
    }
    else
        return new $tr_Result([s, a]);
};

// --- IO ---
var $hs_waitReadzh = function(fd, s) {
    var f = $hs_allFiles[fd];
    if(f.text.length > f.fptr)
        return s;
    f.waitingToRead.push($tr_Scheduler.currentThread);
    return new $tr_Suspend(function(_) { return s; }, [s]);
};
var $hs_waitWritezh = function(fd, s) {
    return s;
//    var f = $hs_allFiles[fd];
//    f.waitingToWrite.push($tr_Scheduler.currentThread);
//    return new $tr_Suspend(function(_) { return s; }, [s]);
};
function getGCStats(a) {
    var sc = $tr_Scheduler;
    var t = new Date().getTime() - sc.currentRunStart + sc.cpuSeconds;
    if(WORD_SIZE_IN_BITS==32) {
        throw "Todo identify the correct offset"
        return 0;
    }
    else {
        (new Float64Array(a[0],a[1]+128))[0] = t/1000;
        return goog.math.Long.ZERO;
    }
};
