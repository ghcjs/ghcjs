/* Thread.js
 * A library for threading using JavaScript 1.7 generators and trampolining.
 * Author: Neil Mix, neilmix [at] gmail [dot] com, http://www.neilmix.com/
 *
 * Please feel free to use this code in any way that pleases you.  If
 * appropriate, drop me an email because I like hearing about interesting 
 * projects.
 */
 
/* special yield value which tells a Thread to suspend execution */
SUSPEND      = { toString: function() { return "[object SUSPEND]" } };

/* special yield value which tells a Thread to send a continuation callback
 * for resuming a thread */
CONTINUATION = { toString: function() { return "[object CONTINUATION]" } };

/* special yield value which tells a Thread to send the Thread object itself */
THREAD       = { toString: function() { return "[object THREAD]" } };
 
/* The thread constructor.
 * The parameter is either a function that creates a generator or a
 * generator itself.  For example, you can seed a Thread with the initial
 * call to a generator function:
 *   var t = new Thread(
 *     functionThatYields()
 *   );
 * Or you can seed the thread by dynamically creating a generator function:
 *   var t = new Thread( function() {
 *     // do some stuff
 *     yield SUSPEND;
 *     // do more stuff
 *   });
 */
function Thread(init) {
	if (typeof(init) == "function") {
		this._init = init();
	} else {
		this._init = init;
	}
	
	this._stack  = [];
	this._state = "run"
	this.waitingThreads = [];

	var _this = this;
	this.resumeDelegate = function(retval) {
		_this.resume(retval);
	}
}

Thread.prototype = {
	constructor: Thread,
	
	/* Start a thread's execution.
	 * As a convenience, returns the thread so you can do:
	 *   var t = new Thread(functionThatYields()).start();
	 */
	start: function() {
		this.resume(this._init);
		return this;
	},

	/* Resumes a Thread with a return value after it has suspended. */
	resume: function(retval) {
		this._run(retval, false);
	},
	
	/* Resumes a Thread with an exception after it has suspended. */
	except: function(e) {
		this._run(e, true);
	},
	
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
			this.waitingThreads.push( (yield THREAD) );
			yield SUSPEND;
		} else {
			yield this.value();
		}
	},
	
	// Executes a Thread using trampolining.
	_run: function(retval, isException) {
		while (true) {
			var method;
			var arg = undefined;
			if (isException) {
				this._stack.pop().close();
				if (this._stack.length) {
					// propagate the exception down the stack
					method = "throw";
					arg    = retval;
				} else {
					// we're done
					this._state = "throw";
					this._value = retval;
					this._signal();
					return;
				}
			} else if (retval == THREAD) {
				// generator is requesting this thread object
				method = "send";
				arg    = this;
			} else if (retval == CONTINUATION) {
				// generator is requesting our resume callback
				method = "send";
				arg    = this.resumeDelegate;
			} else if (retval == SUSPEND) {
				// generator has requested we suspend
				return;
			} else if (retval != null && 
				typeof(retval) == "object" &&
				typeof(retval.next) == "function" && 
				typeof(retval.send) == "function")
			{
				// it's a generator that was returned.
				// add it as a new frame on the stack.
				this._stack.push(retval);
				method = "next";
			} else {
				// regular return value
				// end the current frame
				this._stack.pop().close();
				if (this._stack.length) {
					// return to the previous frame
					method = "send";
					arg    = retval;
				} else {
					// we're done.
					this._state = "return";
					this._value = retval;
					this._signal();
					return;
				}
			}
			try {
				retval = this._stack[this._stack.length-1][method](arg);
				isException = false;
			} catch(e if e == StopIteration) {
				// since a normal return results in StopIteration, we'll
				// just treat this as a return
				retval = undefined;
				isException = false;
			} catch(e) {
				retval = e;
				isException = true;
			}
		}
	},
	
	// Notify joining threads that this thread is complete.
	_signal: function() {
		for (var i = 0; i < this.waitingThreads.length; i++) {
			this.waitingThreads[i]._run(this._value, this._state == "throw");
		}
	}
}
