/**
 * @define {boolean} HS_DEBUG is like goog.DEBUG, but for ghcjs internals
 */
var HS_DEBUG = true;

/**
 * @define {boolean} enable weak pointers and finalizers
 */
var HS_WEAKS = false;

/**
 * @define {boolean} enable traceLog in the run loop
 */
var HS_TRACE = true;

/**
 * @define {boolean} enable tracing in hscall
 */
var HS_TRACE_CALLS = false;

/**
 * @define {boolean} enable include args in the hscall messages
 */
var HS_TRACE_ARGS = false;

/**
 * @define {boolean} enable tracing of RTS calls
 */
var HS_RTS_TRACE = false;

/**
 * @define {boolean} enable the trampoline calling convention
 */
var HS_TRAMPOLINE = true;

/**
 * @define {number} size of Word and Int. If 64 we use goog.math.Long.
 */
var WORD_SIZE_IN_BITS = 64;

