/*
   This script is executed in the developer tools window after startup
   of the Electron application
 */

////////////////////////////////////////////////////////////////////////
//
// some settings

// InspectorBackend.Options.dumpInspectorProtocolMessages = false;
// Protocol.InspectorBackend.Options.dumpInspectorProtocolMessages = true;
// InspectorBackend.Options.dumpInspectorTimeStats = false;

const tracingEnabled = false;
const traceTiming    = false;

////////////////////////////////////////////////////////////////////////
//
// some utilities

// allow debugging in plain node.js
var isNode = false;
if(typeof InspectorFrontendHost === "undefined") {
  global["InspectorFrontendHost"] =
        { events: { dispatchEventToListeners: function() {}
        }
        };
  global["InspectorFrontendHostAPI"] =
        { Events: {}
        };
  global["window"] = {};
  isNode = true;
}

////////////////////////////////////////////////////////////////////////
//
// inspector protocol message interception

var origFrontendDispatch =
  InspectorFrontendHost.events.dispatchEventToListeners;
InspectorFrontendHost.events.dispatchEventToListeners =
   function(evType, evVal) {
     try {
       if(evType === InspectorFrontendHostAPI.Events.DispatchMessage) {
         return rewriteIncomingMessage(evVal, origFrontendDispatch.bind(this));
       } else if(evType === InspectorFrontendHostAPI.Events.DispatchMessageChunk) {
         return rewriteIncomingMessageChunk(evVal, origFrontendDispatch.bind(this));
       } else {
         return origFrontendDispatch.call(this, evType, evVal);
       }
     } catch(e) {
       console.error(e);
       console.error(e.stack);
       throw e;
     }
   };

var origFrontendSendToBackend =
  InspectorFrontendHost.sendMessageToBackend;
InspectorFrontendHost.sendMessageToBackend = function(msg) {
  return rewriteOutgoingMessage(msg, origFrontendSendToBackend.bind(this));
};

////////////////////////////////////////////////////////////////////////
//
// rewrite protocol messages

/*
  process a single incoming message, call the dispatch callback with the
  updated event
 */
function rewriteIncomingMessage(msg, dispatch) {
  trace("incoming: ", msg);
  // message for the old "JavaScript Profiler" pane

  /*
    nodes for the old JavaScript Profiler point to their children instead
    of their parent.

    the CCS unrolling code doesn't support this yet, so we disable handling
    these messages for now. I don't know if anyone still needs the old
    profiler.
   */
  /*
  if(msg.result && msg.result.profile) {
    trace("got profiling message", msg);
    updateCostCentres(function() {
      trace("cost centres updated");
      processProfilingMessage(msg, true);
      trace("profiling data processed", msg);
      dispatch(InspectorFrontendHostAPI.Events.DispatchMessage, msg);
    });
    return;
  }
  */

  // messages for the "Performance" pane
  if(msg.method && msg.method.startsWith("Tracing.")) {
    processTracingMessage(msg, dispatch);
    return;
  }
  // no-op: forward other messages without changes
  return dispatch(InspectorFrontendHostAPI.Events.DispatchMessage, msg);
}

/*
  our knowledge of cost centres and cost centre stacks from the
  Haskell runtime
 */
var costCentres = [];
var costCentreStacks = [];
var costCentresIndex = {}; // cc id -> index in costCentres
var costCentreStacksIndex = {}; // ccs id -> index in costCentreStacks

/*
  when we process a profiling message and encounter an unknown
  cost centre stack id, we refresh our cost centre information.

  when a cost centre stack id is still unavailable after a refresh,
  we mark it as unavailable.

  an unavailable cost centre stack id will not cause further refreshes.
 */
var costCentreStacksUnavailable = {}; // ccs id -> bool

var nodesIndex = {}; // node id -> node object

// fixme: rewrite Haskell source paths to urls somehow?
var baseURL = "";

/*
expand cost centre stacks in a profile message

the profiler gets call stacks of the form

  someFunction
  h$_prof_ccs_b_n
  anonymous
  h$_prof_ccs_b_o
  anonymous
  h$_prof_ccs_b_p
  anonymous
  h$_prof_frame_50
  h$_prof_frame_49
  ...
  h$_prof_frame_2
  h$_prof_frame_1
  someHaskellFunction

where n, o, p are numbers in [0..1023]

these numbers encode a cost centre stack id given by

ccsid = (n << 20) + (o << 10) + p

the Haskell program collects the cost centre data in the global
h$_prof_costCentres and h$_prof_costCentreStacks arrays.

example data

   costCentres: id -> costCentre
     registerCC:  { "id": 2
                  , "name": "main"
                  , "file": "main.hs"
                  , "line": 10
                  , "col": 1
                  }
   costCentreStacks: id -> costCentreStack
     registerCCS: { "id": 3
                  , "ccs": [2,0,1]
                  }

   profile node (in msg.result.profile.nodes array)
      { "id": 37
      , "callFrame":
          { "functionName": "someFunction"
          , "scriptId": "107"
          , "url": "http://localhost:4000/test.js"
          , "lineNumber": 51
          , "columnNumber":20
          }
      , "hitCount":0
      , "children":[38]
      }

 */

/*
  We have to dispatch the tracing messages in the same order as we
  receive them.

  Some messages require a refresh of our cost centre information,
  which means waiting for the result of running some JS code on the
  backend.

  Other tracing messages received in the meantime are queued here.
 */
var tracingMessages = [];
var tracingMessageWaiting = false;
function processTracingMessage(msg, dispatch) {
  trace("processTracingMessage");
  tracingMessages.push({m:msg,d:dispatch});
  processTracingMessageQueue();
}

function processTracingMessageQueue() {
  trace("processTracingMessageQueue");
  while(tracingMessages.length > 0 && !tracingMessageWaiting) {
    var { m: msg, d: dispatch } = tracingMessages.shift();
    if(msg.method === "Tracing.dataCollected" &&
       msg.params.value.findIndex(v => v.name === "ProfileChunk") !== -1) {
      trace("got new profiling message", msg);
      if(processTracingDataMessage(msg, false)) {
        // message fully processed, we can dispatch it
        trace("profiling message processed completely", msg);
        dispatch(InspectorFrontendHostAPI.Events.DispatchMessage, msg);
      } else {
        // message not fully processed, refresh cost centres and retry
        trace("refresh needed");
        tracingMessageWaiting = true;
        updateCostCentres(function() {
          processTracingDataMessage(msg, true);
          trace("profiling message processed after refresh", msg);
          dispatch(InspectorFrontendHostAPI.Events.DispatchMessage, msg);
          tracingMessageWaiting = false;
          processTracingMessageQueue();
        });
        return;
      }
    } else {
      dispatch(InspectorFrontendHostAPI.Events.DispatchMessage, msg);
    }
  }
}

/*
  refresh our knowledge of the cost centres and cost centre stacks
  by retrieving the values of the h$_prof_costCentres and
  h$_prof_costCentreStacks arrays.

  If we already know some cost centre information, we retrieve only
  the part that is new.

  Cost centre data is expected to be immutable:

  New cost centres and stacks can be added, but existing data cannot be changed.
 */
function updateCostCentres(cb) {
  var ccl  = costCentres.length;
  var ccsl = costCentreStacks.length;
  if(ccl > 0) { cb(); return; } // fixme
  trace("updating cost centres: " + ccl + " " + ccsl);
  var js = `JSON.stringify({ cc:  h$_prof_costCentres.slice(${ccl})` +
                          `, ccs: h$_prof_costCentreStacks.slice(${ccsl})` +
                          " })";
  updateCCqueue = [cb];
  evaluateOnBackend(js
    , function(jo) {
      if(!jo) {
        trace("did not find cost centres, maybe no Haskell program running?");
      } else {
        var o = JSON.parse(jo);
        var i;
        if(o.cc) {
          for(i=0;i<o.cc.length;i++) {
            costCentresIndex[o.cc[i].id] = costCentres.length + i;
          }
          costCentres = costCentres.slice(0, ccl).concat(o.cc);

        }
        if(o.ccs) {
          for(i=0;i<o.ccs.length;i++) {
            costCentreStacksIndex[o.ccs[i].id] = costCentreStacks.length + i;
          }
          costCentreStacks = costCentreStacks.slice(0, ccsl).concat(o.ccs);
        }
      }
      trace("cost centres updated", costCentres, costCentreStacks);
      cb();
    });
}

var lastTrace = 0;
function trace() {
  if(!tracingEnabled) return;
  if(traceTiming) {
    var t  = performance.now();
    var dt = lastTrace == 0 ? 0 : t - lastTrace;
    lastTrace = t;
    console.log("dt: " + dt);
  }
  return console.log.apply(console,
    Array.from(arguments, x =>
      typeof x === "object" ? JSON.stringify(x) : x ));
}

// returns false if the message is not fully processed
function processTracingDataMessage(msg, refreshed) {
  try {
    var vals = msg.params.value;
    for(var i = 0; i < vals.length; i++) {
      var val = vals[i];
      if(val.name === "ProfileChunk") {
        if(val.args && val.args.data && val.args.data.cpuProfile) {
          var prof = val.args.data.cpuProfile;
          if(prof.nodes) {
            var newNodes = processProfilingNodes(prof.nodes, refreshed);
            if(!newNodes) return false;
            prof.nodes = newNodes;
          }
        }
      }
    }
    return true;
  } catch(e) {
    console.error(e);
    console.error(e.stack);
  }
  return false;
}

/*
  handling of old profiling messages disabled for now, since the nodes
  have a different format (list of children instead of parent pointer)

function processProfilingMessage(msg, refreshed) {
  try {
    msg.result.profile.nodes = processProfilingNodes(msg.result.profile.nodes, refreshed);
  } catch(e) {
    console.error(e);
    console.error(e.stack);
  }
}
 */

// returns null if the nodes data hasn't been fully refreshed
function processProfilingNodes(nodes, refreshed) {
  trace("processProfilingNodes", nodes);
  var i, j, n, c;
  for(i=0;i<nodes.length;i++) {
    n = nodes[i];
    nodesIndex[n.id] = n;
  }
  for(i=0;i<nodes.length;i++) {
    n = nodes[i];
    trace("inspecting node: " + n.callFrame.functionName);
    if(n.callFrame.functionName == "h$_prof_frame_1") {
      // collect the node ids where we can unroll the cost centre stack
      var frames = [n.id];
      var count = 0;
      var ccsid = 0;
      var ccsparent = -1;
      /*
         we expand the cost centre stack over the h$_prof_frame_x stack
         frames (and possibly nodes with another name in between)
         up to the first h$_prof_ccs_b_y frame we encounter

         The h$_prof_ccs_b_y nodes are shared between the cost centre
         ids (when they have the same parent). We don't want to change
         their parent, since that could disconnect parts of the tree,
         resulting in an error from the profiler.

         Instead we just change the parent of the highest node in our cost
         centre stack to the parent of the highest h$_prof_ccs_b node we
         encounter.
      */
      while(n.parent) {
        n = nodesIndex[n.parent];
        var ccsid_part = getCCId(n.callFrame);
        if(ccsid_part >= 0) {
          ccsid += ccsid_part << (10 * count++);
          if(count === 3) {
            ccsparent = n.parent;
            break;
          }
        }
        if(count === 0) frames.push(n.id);
      }
      trace("expanding cost centre stack id: " + ccsid);
      // expanding the stack
      var stackidx = costCentreStacksIndex[ccsid];
      if(typeof stackidx !== "number") {
        if(refreshed) costCentreStacksUnavailable[ccsid] = true;
        if(costCentreStacksUnavailable[ccsid]) {
          // not an available ccs id, skip expanding
          trace("ccs id unavailable, skipping");
          continue;
        } else {
          trace("ccs id missing, aborting");
          // may be available after refresh
          // return null to signal incomplete processing and to force refesh
          return null;
        }
      }
      var stack = costCentreStacks[stackidx];
      var ccs = stack.ccs.map(x => costCentres[costCentreStacksIndex[x]]);
      if(ccs.length > frames.length) {
        // make new ccs with squeezed frame
        var lastCC = Object.assign({}, ccs[0]);
        for(j=1;j<=ccs.length-frames.length;j++) {
          // we can only preserve the name
          // keep line numbers from the first part
          lastCC.name = ccs[j].name +";" + lastCC.name;
        }
        ccs = ccs.slice(ccs.length-frames.length+1);
        ccs.unshift(lastCC);
        trace("squeezed stack: ", ccs);
      }
      trace("relink parent: ", nodesIndex[frames[ccs.length-1]], ccsparent);
      nodesIndex[frames[ccs.length-1]].parent = ccsparent;
      for(j=0;j<ccs.length;j++) {
        n = nodesIndex[frames[j]];
        c = ccs[j];
        trace("j:" + j + " n: " + n.callFrame.functionName + " c: " + c.name);
        // fixme should we move hitCount (if nonzero)?
        n.callFrame.functionName = "\\" + c.name;
        n.callFrame.lineNumber   = c.line;
        n.callFrame.columnNumber = c.col;
        n.callFrame.url          = baseURL + c.file;
      }
    }
  }
  return nodes;
}

// get the cost centre component value ([0,1023]) of the callFrame
// returns -1 if the callFrame is not a Haskell cc frame
function getCCId(cframe) {
  var prefix = "h$_prof_ccs_b_";
  if(cframe.functionName.startsWith(prefix)) {
    return parseInt(cframe.functionName.substring(prefix.length));
  } else {
    return -1;
  }
}

/*
  process a partial incoming message, call the dispatch callback with the
  updated event
 */
function rewriteIncomingMessageChunk(msg, dispatch) {
  trace("incoming-chunk: ", msg);

  // no-op: forward the event without changes
  return dispatch(InspectorFrontendHostAPI.Events.DispatchMessageChunk, msg);
}

function rewriteOutgoingMessage(msg, dispatch) {
  trace("outgoing: ", msg);

  // no-op: forward the event without changes
  return dispatch(msg);
}

////////////////////////////////////////////////////////////////////////
//
// some logging. we can log to our own console by sending IPC messages

function showArg(arg) {
  // fixme how do we do remote JSON refs?
  if(arg === undefined) return { type: "string", value: "undefined" };
  if(arg === null) return { type: "string", value: "null" };
  // if(typeof arg ==='boolean') return null;
  if(typeof arg==="number")
    return {type: "number", value: arg, description: ""+arg};
  // type: 'function', className: 'Function', description: 'function source',
  // objectId { injectedScriptId: x, id: y }
  if(typeof arg==="function")
    return { type: "function", className: "Function", description: "<function>" };
  // type: 'object', subtype: 'array', className: 'Array', description: 'Array(n)',
  // preview:
  //   type: 'object',
  //   subtype: 'array',
  //   description: 'Array(1000)'
  //   overflow: true
  //   properties:
  //     [{ name: "0", type: "object", value: "Object"}
  //     ,{ name: "1", type: "object", value: "Object"}
  //     ...
  //     ]


  if(typeof arg ==="string") return { type: "string", value: ""+arg };
  if(typeof arg ==="object")
    return { type: "object", hasChildren: true, value: JSON.stringify(arg /* myStringify_r(arg, 0, 3) */) };
    // return { type: "string", value: JSON.stringify(myStringify_r(arg, 0, 6)) };
  return { type: "string", value: "<unknown>" };
}


// valid msgType: log, debug, info, error, warning, clear, dir, dirxml
//                table, trace, startgroup, StartGroupCollapsed, endgroup
//                assert

function logConsole(msgType, msgArgs) {
  logConsoleRaw(msgType, Array.from(msgArgs, showArg));
}

function logConsoleRaw(msgType, args) {
  var consoleAPICall =
        { method: "Runtime.consoleAPICalled"
          , params:
            { type: msgType
              , args: args
              , executionContextId: 1
              , timestamp: Date.now()
              , stackTrace: { "callFrames": [] }
            }
        };
  origFrontendDispatch.call( InspectorFrontendHost.events
    , InspectorFrontendHostAPI.Events.DispatchMessage
    , consoleAPICall);
}

if(!isNode) {
  console.log   = function()
  { return logConsole.apply(this, ["log", arguments]); };
  console.warn  = function()
  { return logConsole.apply(this, ["warning", arguments]); };
  console.error = function()
  { return logConsole.apply(this, ["error", arguments]); };
  console.info  = function()
  { return logConsole.apply(this, ["info", arguments]); };
  console.trace = function()
  { return logConsole.apply(this, ["trace", arguments]); };
}

////////////////////////////////////////////////////////////////////////

function evaluateOnBackend(expr, cb) {
  var tgt = SDK.targetManager.mainTarget();
  if(tgt) {
    var runtimeModel = tgt.model(SDK.RuntimeModel);
    var ec           = runtimeModel.defaultExecutionContext();
    if(ec) {
      var options =
        { expression: expr
          , objectGroup: "console" // fixme get our own object group?
          , includeCommandLineAPI: true
          , silent: false
          , returnByValue: true // false
          , generatePreview: true
        };
      var result = ec.evaluate( options
        , true /* userGesture */
        , false /* awaitPromise */
      );

      result.then(
        function(value) {
          cb(value.object.value);
        }
        , function(reason) {
          console.error("evaluateOnBackend failed: " + reason);
        });
    }
  }
}
