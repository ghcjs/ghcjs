$yt.Scheduler = {
    waiting : [],
    maxID   : 0,
    runWaiting : function() {
        while (this.waiting.length != 0) {
            var t = this.waiting[0];
            this.activeThread = t[0];
            this.waiting = Array.prototype.slice.call(this.waiting, 1, this.waiting.length);
            t[0].resume(t[1]);
        }
    },
    schedule : function(thread, retval) {
        this.waiting[this.waiting.length] = [thread, retval];
    },
    start : function(thread) {
        this.schedule(thread, thread._init);
        this.maxID++;
        thread.threadID = this.maxID;
        return thread;
    }
};

$yt.trace = function(msg) {
    console.log($yt.Scheduler.activeThread.threadID + " : " + msg);
};

$yt.traceThread = function(msg) {
    $yt.trace(msg);
};

$yt.traceMVar = function(msg) {
    $yt.trace(msg);
};

$yt.traceException = function(msg) {
    $yt.trace(msg);
};

$hs.hscall = function () {
    if (this.arity == arguments.length) { // EXACT and THUNK rules
        yield new $yt.Jump(this.evaluate, this, arguments);
    } else if (this.arity < arguments.length) { // CALLK and TCALL rules
        var remainingArguments = Array.prototype.slice.call(arguments, this.arity, arguments.length);
        arguments.length = this.arity;
        var result = (yield this.evaluate.apply(this, arguments));
        yield new $yt.Jump(result.hscall, result, remainingArguments);
    } else if (arguments.length == 0) { // RETFUN
        yield new $yt.Result(this);
    } else if (this instanceof $hs.Pap) { // PCALL rule, we can bypass this rule by building PAPs of PAPs
        yield new $yt.Jump(this.evaluate, this, arguments);
    } else {
        // PAP2 rule and then RETFUN (jump to continuation)
        yield new $yt.Result(new $hs.Pap (this, arguments));
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
        yield new $yt.Jump(this.object.hscall, this.object, newArguments);
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
        var res = yield this.evaluateOnce.apply(this, []);
        this.evaluate = function () {
            yield new $yt.Result(res);
        };
        yield new $yt.Result(res);
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
        yield new $yt.Result(this);
    }
};

$hs.force = function () {
    var f = arguments[0];
    var args = Array.prototype.slice.call(arguments, 1, arguments.length);
    var t = new $yt.Thread(f.hscall.apply(f, args));
    $yt.Scheduler.start(t);
    $yt.Scheduler.runWaiting();
    return t.value();
};

$hs.Thread = {
    fork : function (a, s) {
        var t = new $yt.Thread(a.hscall(s));
        $yt.Scheduler.start(t);
        $yt.traceThread("fork thread " + t.threadID);
        yield [s, t];
    },    
    forkOn : function (n, a, s) {yield fork(a,s);},
    yieldThread : function (s) {
        $yt.traceThread("yield thread");
        var t = yield $yt.GetThread;
        $yt.Scheduler.schedule(t, null);
        yield $yt.Suspend;
        yield s;
    },
    myThreadId : function (s) {
        var t = yield $yt.GetThread;
        yield [s, t];
    },
    noDuplicate : function (s) { return s; }
};

$hs.MutVar.atomicModify = function (a, b, s) {
    var res = yield b.hscall(a.value);
    a.value = res.data[0];
    yield [s, res.data[1]];
};

$hs.MVar = {
    newMVar : function(s) {
        $yt.traceMVar("newMVar");
        return [s, {value : null, waiting : []}];
    },
    take : function (a, s) {
        if (a.value === null) {
            $yt.traceMVar("take waiting");
            a.waiting[a.waiting.length] = yield $yt.GetThread;
            yield $yt.Suspend;
        }
        $yt.traceMVar("take taking");
        var result = a.value;
        a.value = null;
        if (a.waiting.length != 0) {
            $yt.traceMVar("take waking waiters");
            var w = a.waiting[0];
            a.waiting = Array.prototype.slice.call(a.waiting, 1, a.waiting.length)
            $yt.Scheduler.schedule(w, null);
        }
        yield [s, result];
    },
    tryTake : function (a, s) {
        if (a.value === null) {
            $yt.traceMVar("tryTake nothing to take");
            yield [s, 0, null];
            return;
        }
        $yt.traceMVar("tryTake taking");
        var result = a.value;
        a.value = null;
        if (a.waiting.length != 0) {
            $yt.traceMVar("tryTake waking waiters");
            var w = a.waiting[0];
            a.waiting = Array.prototype.slice.call(a.waiting, 1, a.waiting.length)
            $yt.Scheduler.schedule(w, null);
        }
        yield [s, 1, result];
    },
    put : function (a, b, s) {
        if (a.value !== null) {
            $yt.traceMVar("put waiting");
            a.waiting[a.waiting.length] = yield $yt.Thread;
            yield $yt.Suspend;
        }
        $yt.traceMVar("put putting");
        a.value = b;
        if (a.waiting.length != 0) {
            $yt.traceMVar("put waking waiters");
            var w = a.waiting[0];
            a.waiting = Array.prototype.slice.call(a.waiting, 1, a.waiting.length)
            $yt.Scheduler.schedule(w, null);
        }
        yield s;
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
        try {
            $yt.traceException("trying");
            yield a.hscall(s);
            $yt.traceException("done trying");
        }
        catch(e) {
            $yt.traceException("catching");
            yield b.hscall(e, s);
            $yt.traceException("done catching");
        }
    },
    raise : function(a) {
        $yt.traceException("raise");
        throw a
    },
    raiseIO : function(a, s) {
        $yt.traceException("raiseIO");
        throw a
    }
};

