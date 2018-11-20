////////////////////////////////////////////////////////////////////////
//
// some settings

// InspectorBackend.Options.dumpInspectorProtocolMessages = false;
// Protocol.InspectorBackend.Options.dumpInspectorProtocolMessages = true;
// InspectorBackend.Options.dumpInspectorTimeStats = false;


////////////////////////////////////////////////////////////////////////
//
// some utilities

// allow debugging in plain node.js
var isNode = false;
if(typeof InspectorFrontendHost === 'undefined') {
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


function myStringify(o, n = 20) {
  return JSON.stringify(myStringify_r(o, 0, n));
}

// if the default gives a result that's too big
function myStringify2(o) { return myStringify(o, 2); }
function myStringify3(o) { return myStringify(o, 3); }

function myStringify2a(o) { return myStringify_r(o, 0, 2); }
function myStringify3a(o) { return myStringify_r(o, 0, 3); }

function myStringify_r(o, n, m) {
  if(n > m) return -1; // "max depth";
  var i, r, pv;
  try {
    // if(typeof o === 'function') {
    //  r = "<function>"
    if(Array.isArray(o)) {
      r = [];
      for(i=0;i<o.length;i++) {
        if(i >= 100) break;
        try {
          pv = myStringify_r(o[i], n+1, m)
        } catch(e) {
          pv = e.toString() + e.stack;
        }
        r.push(pv);
      }
    } else if((typeof o === 'object' || typeof o === 'function') && o) {
      r = {};
      if(typeof o === 'function') r.__type = 'function'
      for(var prop in o) {
        try {
          pv = myStringify_r(o[prop],n+1,m);
        } catch(e) {
          pv = e.toString() + e.stack;
        }
        r[prop] = pv;
      }
    } else {
      r = o;
    }
  } catch(e) {
    r = e.toString() + e.stack;
  }
  return r; //{ t: typeof o, v: r };
}

function mySearch_r(o, t, s, r, n, m) {
  if(o === t) { // found
    r.push(s);
    return;
  }
  if(n > m) return; // "max depth";
  try {
    if(Array.isArray(o)) {
      for(var i=0;i<o.length;i++) {
        if(i > 100) return;
        try {
          mySearch_r(o[i], t, s + '[' + i + ']', r, n+1, m)
        } catch(e) { }
      }
    } else if((typeof o === 'object' || typeof o === 'function') && o) {
      for(var prop in o) {
        try {
          mySearch_r(o[prop], t, s + '[\'' + prop + '\']', r, n+1, m);
        } catch(e) { }
      }
    }
  } catch(e) { }
}


function escapeHTML(x) {
    return x.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
}

////////////////////////////////////////////////////////////////////////
//
// "emergency" debug output in an absolute position div

var outpContents;
function addDebugOutput() {
  var showOutput = false;
  var outpToggle = document.createElement('div');
  outpToggle.style.position = "absolute"
  outpToggle.style.right = "15px"
  outpToggle.style.bottom = "0"
  outpToggle.style.width = "15px"
  outpToggle.style.height = "15px"
  outpToggle.style.zIndex = "40005"
  outpToggle.style.background = "black"
  outpToggle.style.opacity = "0.3"
  var outpElem = document.createElement('div');
  outpElem.style.position = "absolute"
  outpElem.style.right = "0"
  outpElem.style.left = "50%"
  outpElem.style.top = "0"
  outpElem.style.bottom = "0"
  outpElem.style.zIndex = "10000"
  outpElem.style.background = "white"
  outpElem.style.display = "none"
  outpContents = document.createElement('div');
  outpContents.style.position = "absolute"
  outpContents.style.right = "0"
  outpContents.style.top = "0"
  outpContents.style.bottom = "0"
  outpContents.style.left = "0"
  outpContents.style.zIndex = "10000"
  outpContents.style.overflow = "scroll"
  outpElem.appendChild(outpContents)
  var outpClear = document.createElement('div');
  outpClear.style.position = "absolute"
  outpClear.style.right = "30px"
  outpClear.style.bottom = "0"
  outpClear.style.width = "15px"
  outpClear.style.height = "15px"
  outpClear.style.zIndex = "10002"
  outpClear.style.background = "red"
  outpClear.style.opacity = "0.3"
  outpElem.appendChild(outpClear);
  var outpSelect = document.createElement('div');
  outpSelect.style.position = "absolute";
  outpSelect.style.right = "45px";
  outpSelect.style.bottom = "0";
  outpSelect.style.width = "15px";
  outpSelect.style.height = "15px";
  outpSelect.style.zIndex = "10002";
  outpSelect.style.background = "blue";
  outpSelect.style.opacity = "0.3";
  outpElem.appendChild(outpSelect);
  outpToggle.addEventListener('click', function() {
    showOutput = !showOutput;
    outpElem.style.display = showOutput ? "block" : "none";
  });
  outpClear.addEventListener('click', function() {
    var n = outpContents.children.length;
    try {
      for(var i = 10; i < n; i++) {
        outpContents.lastElementChild.remove();
      }
    } catch(e) { }
  });
  outpSelect.addEventListener('click', function() {
    var range = document.createRange();
    range.selectNode(outpElem);
    window.getSelection().removeAllRanges();
    window.getSelection().addRange(range);
    outp("### select clicked");
  });
  document.body.appendChild(outpToggle);
  // document.body.appendChild(outpSelect);

  document.body.appendChild(outpElem);
}

function outp(txt) {
  if(!outpContents) addDebugOutput();
  var outpDiv = document.createElement('div');
  outpDiv.style.borderTop = "1px solid #bbb";
  outpDiv.style.padding   = "5px";
  var outpTxt = document.createTextNode(txt);
  outpDiv.appendChild(outpTxt);
  if(outpContents.firstChild) {
    outpContents.insertBefore(outpDiv, outpContents.firstChild);
  } else {
    outpContents.appendChild(outpDiv);
  }
  return outpDiv;
}

////////////////////////////////////////////////////////////////////////
//
// inspector protocol message interception

var origFrontendDispatch =
  InspectorFrontendHost.events.dispatchEventToListeners;
InspectorFrontendHost.events.dispatchEventToListeners =
   function(evType, evVal) {
    if(evType === InspectorFrontendHostAPI.Events.DispatchMessage) {
      return rewriteIncomingMessage(evVal, origFrontendDispatch.bind(this));
    } else if(evType === InspectorFrontendHostAPI.Events.DispatchMessageChunk) {
      return rewriteIncomingMessageChunk(evVal, origFrontendDispatch.bind(this));
    } else {
      return origFrontendDispatch.call(this, evType, evVal);
    }
  };

var origFrontendSendToBackend =
  InspectorFrontendHost.sendMessageToBackend;
InspectorFrontendHost.sendMessageToBackend = function(msg) {
  return rewriteOutgoingMessage(msg, origFrontendSendToBackend.bind(this));
}

////////////////////////////////////////////////////////////////////////
//
// rewrite protocol messages

// process a single incoming message, call the dispatch callback with the updated event
function rewriteIncomingMessage(msg, dispatch) {
  outp("incoming: " + JSON.stringify(msg));
  if(msg.result && msg.result.profile) {
    function logProfMsg(m) {
      var msg2 = JSON.parse(JSON.stringify(m));
      msg2.result.profile.samples = []; // make it smaller
      msg2.result.profile.timeDeltas = [];
      msg2 = JSON.stringify(msg2); // debug!
      logConsoleRaw("log", [{ type: "object", hasChildren: true, value: JSON.stringify(msg2)}])
    }
    logProfMsg(msg);
    // try {
    lo("got profiling message");
    updateCostCentres(function() {
      lo("cost centres updated");
      processProfilingMessage(msg);
      lo("profiling data processed");
      logProfMsg(msg);
      dispatch(InspectorFrontendHostAPI.Events.DispatchMessage, msg);
    });
    return;
  // } catch(e) {
    //console.log(e); // .toString());
  //}
    // logConsoleRaw("log", [{ type: "object",  hasChildren: true, value: JSON.stringify(msg) }]);
  }

  // no-op: forward the event without changes
  return dispatch(InspectorFrontendHostAPI.Events.DispatchMessage, msg);
}

var costCentres = {};
var costCentreStacks = {};
// fixme: rewrite Haskell source paths to urls somehow?
var baseURL = '';

/*
expand cost centre stacks in a profile message

the profiler gets cost stacks of the form

  someFunction
  h$_cc_n
  anonymous
  h$_cc_o
  anonymous
  h$_cc_p
  anonymous
  someHaskellFunction

where n, o, p are numbers in [0..1023]

these numbers encode a cost centre stack id given by

ccsid = (n << 20) + (o << 10) + p

the Haskell program should have reported the cost centres and
cost centre stacks by sending registerCC and registerCCS messages

here we expand the stacks that we can find

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
function updateCostCentres(cb) {
  evaluateOnBackend
    ( "JSON.stringify({ cc: h$_prof_costCentres, ccs: h$_prof_costCentreStacks })"
    , function(jo) {
        if(!jo) {
          lo("did not find cost centres, maybe no Haskell program running?");
        } else {
          lo("got reported cost centres:");
          lo(jo);
          var o = JSON.parse(jo);
          var i;
          if(o.cc) {
          for(i=0;i<o.cc.length;i++) {
            costCentres[o.cc[i].id] = o.cc[i];
          }
        }
        if(o.ccs) {
          for(i=0;i<o.ccs.length;i++) {
            costCentreStacks[o.ccs[i].id] = o.ccs[i];
          }
        }
        lo("cost centres updated");
      }
      cb();
  });
}

var lastLo = 0;
function lo() {
  var t  = performance.now();
  var dt = lastLo == 0 ? 0 : t - lastLo;
  lastLo = t;
  console.log("dt: " + dt);
  return console.log.apply(console, arguments);
}
function processProfilingMessage(msg) {
  try {
    processProfilingMessage0(msg);
  } catch(e) {
    lo(e);
    lo(e.stack);
  }
}

function processProfilingMessage0(msg) {
  var nodes = msg.result.profile.nodes;
  var maxId = 0;
  var i, j, k, l;
  var n, n_i, n_j, n_k, ccs_i, ccs_j, ccs_k;
  var nodesIndex = {}; // nodeId -> index in original nodes array
  for(i=0;i<nodes.length;i++) {
    var n = nodes[i];
    if(n.id > maxId) maxId = n.id;
    nodesIndex[n.id] = i;
  }
  lo("known cost centres: " + JSON.stringify(costCentres));
  lo("known cost centre stacks: " + JSON.stringify(costCentreStacks));
  lo("nodes: " + nodes.length + " maxId: " + maxId);
  // remove intermediate "anonymous" nodes between h$_cc_ nodes
  for(i=0;i<nodes.length;i++) {
    var n_i = nodes[i];
    if(getCCId(n_i.callFrame) >= 0 && n_i.children) {
      var newChildren = [];
      for(j=0;j<n_i.children.length;j++) {
        n_j = nodes[nodesIndex[n_i.children[j]]];
        if(n_j.callFrame.functionName == "") {
          lo("remove children: " + i + " " + j + " " + n_i.callFrame.functionName + " " + n_j.callFrame.functionName);
          newChildren = newChildren.concat(n_j.children);
          n_i.hitCount += n_j.hitCount;
        } else {
          newChildren.push(n_i.children[j]);
        }
      }
      n_i.children = newChildren;
    }
  }
  // expand h$_cc_n triplets to the full cost centre stack
  var newNodes = [];
  var nextId = maxId+1;
  var nodesReplace = {};

  // append cost centre stacks to triplets
  for(i=0;i<nodes.length;i++) {
    var n_i = nodes[i];
    if((ccs_i = getCCId(n_i.callFrame)) !== -1 && n_i.children) {
      for(j=0;j<n_i.children.length;j++) {
        var n_j = nodes[nodesIndex[n_i.children[j]]];
        if((ccs_j = getCCId(n_j.callFrame)) !== -1 && n_j.children) {
          for(k=0;k<n_j.children.length;k++) {
            var n_k = nodes[nodesIndex[n_j.children[k]]];
            if((ccs_k = getCCId(n_k.callFrame)) !== -1) {
              var ccsid = (ccs_i << 20) + (ccs_j << 10) + ccs_k;
              lo("found encoded CCS: " + ccs_i + " " + ccs_j + " " + ccs_k + " -> " + ccsid);
              lo("  at: " + n_i.id + " " + n_j.id + " " + n_k.id);
              // start the cost centre stack at node i
              // and add the children of k to it
              var ccs = costCentreStacks[ccsid];
              var cc;
              if(ccs && ccs.ccs.length > 0) {
                var missing = false;
                var firstNodeId = nextId;
                for(l=1;l<ccs.ccs.length;l++) {
                  var ccid = ccs.ccs[l];
                  var hcc  = costCentres[ccid];
                  if(hcc) {
                    cc = { id: nextId++
                         , callFrame:
                             { functionName: /* "\u03BB= "  + */ '\\' + hcc.name
                               /* should we take the scriptId the compiled
                                  Haskell source perhaps? do we need a script
                                  per file?
                                */
                             , scriptId: 0
                             , url: baseURL + hcc.file
                             , lineNumber: hcc.line
                             , columnNumber: hcc.col
                             }
                         /* we could distribute the hitcount of the nodes
                            we're replacing, but we're not expecting
                            many hits inside the nodes. */
                         , hitCount: 0
                         , children: []
                         };
                    if(l == ccs.ccs.length-1) {
                      // last node, add original children
                      if(n_k.children) {
                        cc.children = cc.children.concat(n_k.children);
                      }
                    } else {
                      // inner node, add next node
                      cc.children.push(nextId);
                    }
                    lo("created cc node: " + cc.id + " -> " + cc.callFrame.functionName + " [" + cc.children.join(",") + "]");
                    newNodes.push(cc);
                    // here
                  } else {
                    lo("cost centre not found: " + ccid);
                    missing = true;
                  }
                } // cc stack loop
                /* if all nodes were found, replace references to
                   the first node in our triplet with the new stack
                 */
                if(!missing) {
                  // lo("replacing: " + n_i.id + " -> " + firstNodeId);
                  // n_k.children = [firstNodeId];
                  nodesReplace[n_k.id] = [firstNodeId];
                }
              } else {
                lo("cost centre stack not found: " + ccsid);
              } // ccs found
            } // k is valid h$_cc_ element
          } // k loop
        } // j is valid h$_cc_ element
      } // j loop
    } // i is valid h$_cc_ element
  } // i loop ???

  for(i=0;i<nodes.length;i++) {
    n_i = nodes[i];
    var nr = nodesReplace[n_i.id];
    if(nr && typeof nr.length == 'number') {
      lo("updating children of " + n_i.id);
      n_i.children = nr;
    }
  }
  nodes = nodes.concat(newNodes);
  for(i=0;i<nodes.length;i++) {
    var n = nodes[i];
    nodesIndex[n.id] = i;
  }

  // remove triplets
  var removed; // = false;
  do {
    removed = false;
    for(i=0;i<nodes.length;i++) {
      var n_i = nodes[i];
      if(n_i.children) {
        var newChildren = [];
        for(j=0;j<n_i.children.length;j++) {
          n_j = nodes[nodesIndex[n_i.children[j]]];
          if(getCCId(n_j.callFrame) !== -1) {
            removed = true;
            if(n_j.children) {
              newChildren = newChildren.concat(n_j.children);
            }
          } else {
            newChildren.push(n_i.children[j]);
          }

          /*while(getCCId(n_j.callFrame) !== -1 && n_j.children) {
            n_j = nodes[nodesIndex[n_j.children[0]]];
          }
          n_i.children[j] = n_j.id; */

        }
        n_i.children = newChildren;
      }
    }
  } while(removed);
/*
    if(getCCId(n_i.callFrame) >= 0 && n_i.children) {
      var newChildren = [];

        lo("remove tripl idx: " + i + " " + j + " ids: " + n_i.id);

        if(getCCId(n_j.callFrame) !== -1) {
          lo("remove children: " + i + " " + j + " " + n_i.callFrame.functionName + " " + n_j.callFrame.functionName);
          newChildren = newChildren.concat(n_j.children);
          n_i.hitCount += n_j.hitCount;
        } else {
          newChildren.push(n_i.children[j]);
        }
      }
      n_i.children = newChildren;
    }
  }
*/
  // do the replacement
  /*
  for(i=0;i<nodes.length;i++) {
    n_i = nodes[i];
    for(j=0;j<n_i.children.length;j++) {
      var rch = nodesReplace[n_i.children[j]];
      if(typeof rch === 'number') n_i.children[j] = rch;
    }
  } */
  // fixme should we reattribute samples?

  // and add the new nodes to the message
  msg.result.profile.nodes = nodes; // concat(newNodes);
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

// process a partial incoming message, call the dispatch callback with the updated event
function rewriteIncomingMessageChunk(msg, dispatch) {
  outp("incoming-chunk: " + JSON.stringify(msg));

  // no-op: forward the event without changes
  return dispatch(InspectorFrontendHostAPI.Events.DispatchMessageChunk, msg);
}

function rewriteOutgoingMessage(msg, dispatch) {
  outp("outgoing: " + JSON.stringify(msg));
  // outp("stack: " + new Error().stack);

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
  if(typeof arg==='number')
    return {type: 'number', value: arg, description: ''+arg};
  // type: 'function', className: 'Function', description: 'function source',
  // objectId { injectedScriptId: x, id: y }
  if(typeof arg==='function')
    return { type: "function", className: 'Function', description: "<function>" };
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


  if(typeof arg ==='string') return { type: "string", value: ''+arg };
  if(typeof arg ==='object')
    return { type: "object", hasChildren: true, value: JSON.stringify(myStringify_r(arg, 0, 3)) };
    // return { type: "string", value: JSON.stringify(myStringify_r(arg, 0, 6)) };
  return { type: "string", value: "<unknown>" };
}


// valid msgType: log, debug, info, error, warning, clear, dir, dirxml
//                table, trace, startgroup, StartGroupCollapsed, endgroup
//                assert

function logConsole(msgType, msgArgs) {
  var args = [];
  for(var i = 0; i < msgArgs.length; i++) args.push(showArg(msgArgs[i]));
  logConsoleRaw(msgType, args);
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
  { return logConsole.apply(this, ["log", arguments]); }
console.warn  = function()
  { return logConsole.apply(this, ["warning", arguments]); }
console.error = function()
  { return logConsole.apply(this, ["error", arguments]); }
console.info  = function()
  { return logConsole.apply(this, ["info", arguments]); }
console.trace = function()
  { return logConsole.apply(this, ["trace", arguments]); }
}

////////////////////////////////////////////////////////////////////////
//

function replEvalJS(code) {
  try      { console.log(eval(code)); }
  catch(e) { console.error(e); }
}

function _replEvalJS(code) {
  try      { eval(code); }
  catch(e) { }
}

// evaluate expr:string
// callback:function called with result obj:
//    result.description
//    result.hasChildren
//    result.type
//    result.runtimeAgent
//    result.runtimeModel
//    result.type
//    result.value
function evaluateOnBackend(expr, cb) {
  // outp("evaluateOnBackend: " + expr)
  // outp("evaluateOnBackend: " + (typeof expr))
  // var evaluated = false;
  var tgt = SDK.targetManager.mainTarget();
  // outp("check tgt");
  if(tgt) {
    // outp("got tgt");
    var runtimeModel = tgt.model(SDK.RuntimeModel);
    var ec           = runtimeModel.defaultExecutionContext();
    // outp("check ec");
    if(ec) {
      // outp("got ec:" + myStringify2(ec));
      // outp("got ec.evaluate:" + myStringify2(ec.evaluate));
      // console.log(myStringify3a(ec));
      // console.log(ec.evaluate.toString());
      // console.log(ec._evaluateGlobal.toString());

      // API has changed, options object in newer versions?
      // options.expression
      // expression
      var options =
        { expression: expr
        , objectGroup: "console" // fixme get our own object group?
        , includeCommandLineAPI: true
        , silent: false
        , returnByValue: true // false
        , generatePreview: true
      };
      // lo("options: " + JSON.stringify(options));
      // lo("" + ec);
      // lo("" + ec.evaluate);
    /*  var result = ec.evaluate
        ( options.expression
        , options.objectGroup
        , options.includeCommandLineAPI
        , options.silent
        , options.returnByValue
        , options.generatePreview
        , */ /* userGesture */ /* true
        // , /* awaitPromise */ /* false
        , cb); */

      var result = ec.evaluate( options
                              , true /* userGesture */
                              , false /* awaitPromise */
                              )

      console.log("got result: " + result);
      result.then(
        function(value) {
          console.log(value.object);
          console.log("got value: " + value.object.value);
          // console.log(value);
          cb(value.object.value);
        }
        , function(reason) {
          lo("evaluateOnBackend failed: " + reason);
      });
      // outp("got result:" + myStringify2(result));

      /*then(function() {
        outp("result: " + JSON.stringify(arguments));
      }).catch(function() {
        outp("error: " + JSON.stringify(arguments));
      }) ;
      outp("result: " + result); */
      // evaluated = true;
    }
  }
}

// debug
function test0() {
  outp("test0");
  try {
    return evaluateOnBackend("/* test0 */ 6+12", function(result){
      logConsoleRaw("log", [result]);
    });
  } catch(e) {
    outp("error: " + e);
    return "fail";
  }
}

function initHaskellTools() {

}

var ranTest = 0;
function testMsg() {
  var t = Date.now();
  if(t - ranTest > 5000) {
    //if(!ranTest) {
      console.log("this is a test");
    // outp("this is a test too");

      test0();
      ranTest = t;
  }
}

// XXX
//setTimeout(testMsg, 1000);


// some offline tests
if(isNode) {
  // same API as the external profiling module
  var profiling =
    { registerCC:
        function(id, name, file, line, col) {
          costCentres[id] = { id: id, name: name, file: file, line: line, col: col };
        }
    , registerCCS:
        function(id, ccs) {
          costCentreStacks[id] = { id: id, ccs: ccs }
        }
    };

  profiling.registerCC(0, "CostCentre1", "some/haskell/file.hs", 1, 1);
  profiling.registerCC(1, "CostCentre2", "some/other/haskell/file.hs", 20, 40);
  profiling.registerCC(2, "main", "main.hs", 10, 1);
  profiling.registerCCS(0, [2]);
  profiling.registerCCS(1, [2,1]);
  profiling.registerCCS(2, [2,0]);
  profiling.registerCCS(3, [2,0,1]);
  profiling.registerCCS(4, [2,0,1,0]);

  var profMsgTxt =
  "{\"id\":106,\"result\":{\"profile\":{\"nodes\":[{\"id\":1,\"callFrame\":{\"functionName\":\"(root)\",\"scriptId\":\"0\",\"url\":\"\",\"lineNumber\":-1,\"columnNumber\":-1},\"hitCount\":0,\"children\":[2,3,4]},{\"id\":2,\"callFrame\":{\"functionName\":\"(program)\",\"scriptId\":\"0\",\"url\":\"\",\"lineNumber\":-1,\"columnNumber\":-1},\"hitCount\":18},{\"id\":3,\"callFrame\":{\"functionName\":\"(idle)\",\"scriptId\":\"0\",\"url\":\"\",\"lineNumber\":-1,\"columnNumber\":-1},\"hitCount\":212},{\"id\":4,\"callFrame\":{\"functionName\":\"\",\"scriptId\":\"107\",\"url\":\"http://localhost:4000/test.js\",\"lineNumber\":169,\"columnNumber\":23},\"hitCount\":0,\"children\":[5]},{\"id\":5,\"callFrame\":{\"functionName\":\"testProf1\",\"scriptId\":\"107\",\"url\":\"http://localhost:4000/test.js\",\"lineNumber\":164,\"columnNumber\":18},\"hitCount\":1,\"children\":[6,51],\"positionTicks\":[{\"line\":165,\"ticks\":1}]},{\"id\":6,\"callFrame\":{\"functionName\":\"doCb\",\"scriptId\":\"109\",\"url\":\"/Users/luite/haskell/ghcjs-dev/ghcjs-dev/lib/ghcjs-profiling-new/lib/main.js\",\"lineNumber\":264,\"columnNumber\":22},\"hitCount\":0,\"children\":[7,14]},{\"id\":7,\"callFrame\":{\"functionName\":\"mkCostCentres\",\"scriptId\":\"109\",\"url\":\"/Users/luite/haskell/ghcjs-dev/ghcjs-dev/lib/ghcjs-profiling-new/lib/main.js\",\"lineNumber\":80,\"columnNumber\":22},\"hitCount\":0,\"children\":[8]},{\"id\":8,\"callFrame\":{\"functionName\":\"getStackTraceOuter\",\"scriptId\":\"0\",\"url\":\"\",\"lineNumber\":-1,\"columnNumber\":-1},\"hitCount\":0,\"children\":[9]},{\"id\":9,\"callFrame\":{\"functionName\":\"\",\"scriptId\":\"109\",\"url\":\"/Users/luite/haskell/ghcjs-dev/ghcjs-dev/lib/ghcjs-profiling-new/lib/main.js\",\"lineNumber\":85,\"columnNumber\":37},\"hitCount\":0,\"children\":[10,12]},{\"id\":10,\"callFrame\":{\"functionName\":\"h$_cc_3\",\"scriptId\":\"111\",\"url\":\"\",\"lineNumber\":6,\"columnNumber\":16},\"hitCount\":1,\"children\":[11],\"positionTicks\":[{\"line\":7,\"ticks\":1}]},{\"id\":11,\"callFrame\":{\"functionName\":\"getStackTrace\",\"scriptId\":\"0\",\"url\":\"\",\"lineNumber\":-1,\"columnNumber\":-1},\"hitCount\":2,\"positionTicks\":[{\"line\":7,\"ticks\":2}]},{\"id\":12,\"callFrame\":{\"functionName\":\"h$_cc_0\",\"scriptId\":\"111\",\"url\":\"\",\"lineNumber\":3,\"columnNumber\":16},\"hitCount\":1,\"children\":[13],\"positionTicks\":[{\"line\":4,\"ticks\":1}]},{\"id\":13,\"callFrame\":{\"functionName\":\"getStackTrace\",\"scriptId\":\"0\",\"url\":\"\",\"lineNumber\":-1,\"columnNumber\":-1},\"hitCount\":9,\"positionTicks\":[{\"line\":4,\"ticks\":9}]},{\"id\":14,\"callFrame\":{\"functionName\":\"h$_cc_0\",\"scriptId\":\"111\",\"url\":\"\",\"lineNumber\":3,\"columnNumber\":16},\"hitCount\":0,\"children\":[15]},{\"id\":15,\"callFrame\":{\"functionName\":\"\",\"scriptId\":\"109\",\"url\":\"/Users/luite/haskell/ghcjs-dev/ghcjs-dev/lib/ghcjs-profiling-new/lib/main.js\",\"lineNumber\":298,\"columnNumber\":35},\"hitCount\":0,\"children\":[16]},{\"id\":16,\"callFrame\":{\"functionName\":\"h$_cc_0\",\"scriptId\":\"111\",\"url\":\"\",\"lineNumber\":3,\"columnNumber\":16},\"hitCount\":0,\"children\":[17]},{\"id\":17,\"callFrame\":{\"functionName\":\"\",\"scriptId\":\"109\",\"url\":\"/Users/luite/haskell/ghcjs-dev/ghcjs-dev/lib/ghcjs-profiling-new/lib/main.js\",\"lineNumber\":299,\"columnNumber\":37},\"hitCount\":0,\"children\":[18,32]},{\"id\":18,\"callFrame\":{\"functionName\":\"h$_cc_3\",\"scriptId\":\"111\",\"url\":\"\",\"lineNumber\":6,\"columnNumber\":16},\"hitCount\":0,\"children\":[19]},{\"id\":19,\"callFrame\":{\"functionName\":\"\",\"scriptId\":\"109\",\"url\":\"/Users/luite/haskell/ghcjs-dev/ghcjs-dev/lib/ghcjs-profiling-new/lib/main.js\",\"lineNumber\":300,\"columnNumber\":39},\"hitCount\":0,\"children\":[20]},{\"id\":20,\"callFrame\":{\"functionName\":\"testProf0\",\"scriptId\":\"107\",\"url\":\"http://localhost:4000/test.js\",\"lineNumber\":148,\"columnNumber\":18},\"hitCount\":0,\"children\":[21]},{\"id\":21,\"callFrame\":{\"functionName\":\"myFun1\",\"scriptId\":\"107\",\"url\":\"http://localhost:4000/test.js\",\"lineNumber\":94,\"columnNumber\":15},\"hitCount\":13,\"children\":[22,23],\"positionTicks\":[{\"line\":97,\"ticks\":4},{\"line\":98,\"ticks\":5},{\"line\":95,\"ticks\":4}]},{\"id\":22,\"callFrame\":{\"functionName\":\"fib\",\"scriptId\":\"107\",\"url\":\"http://localhost:4000/test.js\",\"lineNumber\":85,\"columnNumber\":12},\"hitCount\":9021,\"positionTicks\":[{\"line\":88,\"ticks\":6332},{\"line\":89,\"ticks\":2685},{\"line\":87,\"ticks\":4}]},{\"id\":23,\"callFrame\":{\"functionName\":\"wrapObject\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":27,\"columnNumber\":112},\"hitCount\":0,\"children\":[24]},{\"id\":24,\"callFrame\":{\"functionName\":\"_wrapObject\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":32,\"columnNumber\":91},\"hitCount\":0,\"children\":[25]},{\"id\":25,\"callFrame\":{\"functionName\":\"InjectedScript.RemoteObject\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":123,\"columnNumber\":76},\"hitCount\":0,\"children\":[26,28]},{\"id\":26,\"callFrame\":{\"functionName\":\"_describe\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":95,\"columnNumber\":46},\"hitCount\":0,\"children\":[27]},{\"id\":27,\"callFrame\":{\"functionName\":\"get stack\",\"scriptId\":\"0\",\"url\":\"\",\"lineNumber\":-1,\"columnNumber\":-1},\"hitCount\":1,\"positionTicks\":[{\"line\":113,\"ticks\":1}]},{\"id\":28,\"callFrame\":{\"functionName\":\"_generatePreview\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":149,\"columnNumber\":73},\"hitCount\":0,\"children\":[29]},{\"id\":29,\"callFrame\":{\"functionName\":\"_propertyDescriptors\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":46,\"columnNumber\":119},\"hitCount\":0,\"children\":[30]},{\"id\":30,\"callFrame\":{\"functionName\":\"process\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":47,\"columnNumber\":171},\"hitCount\":1,\"children\":[31],\"positionTicks\":[{\"line\":53,\"ticks\":1}]},{\"id\":31,\"callFrame\":{\"functionName\":\"addPropertyIfNeeded\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":156,\"columnNumber\":43},\"hitCount\":1,\"positionTicks\":[{\"line\":157,\"ticks\":1}]},{\"id\":32,\"callFrame\":{\"functionName\":\"h$_cc_4\",\"scriptId\":\"111\",\"url\":\"\",\"lineNumber\":7,\"columnNumber\":16},\"hitCount\":0,\"children\":[33]},{\"id\":33,\"callFrame\":{\"functionName\":\"\",\"scriptId\":\"109\",\"url\":\"/Users/luite/haskell/ghcjs-dev/ghcjs-dev/lib/ghcjs-profiling-new/lib/main.js\",\"lineNumber\":300,\"columnNumber\":39},\"hitCount\":0,\"children\":[34]},{\"id\":34,\"callFrame\":{\"functionName\":\"testProf0\",\"scriptId\":\"107\",\"url\":\"http://localhost:4000/test.js\",\"lineNumber\":148,\"columnNumber\":18},\"hitCount\":0,\"children\":[35]},{\"id\":35,\"callFrame\":{\"functionName\":\"myFun1\",\"scriptId\":\"107\",\"url\":\"http://localhost:4000/test.js\",\"lineNumber\":94,\"columnNumber\":15},\"hitCount\":11,\"children\":[36,41],\"positionTicks\":[{\"line\":97,\"ticks\":4},{\"line\":98,\"ticks\":2},{\"line\":95,\"ticks\":5}]},{\"id\":36,\"callFrame\":{\"functionName\":\"fib\",\"scriptId\":\"107\",\"url\":\"http://localhost:4000/test.js\",\"lineNumber\":85,\"columnNumber\":12},\"hitCount\":9729,\"children\":[37],\"positionTicks\":[{\"line\":88,\"ticks\":6789},{\"line\":89,\"ticks\":2937},{\"line\":87,\"ticks\":3}]},{\"id\":37,\"callFrame\":{\"functionName\":\"wrapObject\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":27,\"columnNumber\":112},\"hitCount\":0,\"children\":[38]},{\"id\":38,\"callFrame\":{\"functionName\":\"_wrapObject\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":32,\"columnNumber\":91},\"hitCount\":0,\"children\":[39]},{\"id\":39,\"callFrame\":{\"functionName\":\"InjectedScript.RemoteObject\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":123,\"columnNumber\":76},\"hitCount\":0,\"children\":[40]},{\"id\":40,\"callFrame\":{\"functionName\":\"isPrimitiveValue\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":25,\"columnNumber\":252},\"hitCount\":1,\"positionTicks\":[{\"line\":26,\"ticks\":1}]},{\"id\":41,\"callFrame\":{\"functionName\":\"wrapObject\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":27,\"columnNumber\":112},\"hitCount\":0,\"children\":[42]},{\"id\":42,\"callFrame\":{\"functionName\":\"_wrapObject\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":32,\"columnNumber\":91},\"hitCount\":0,\"children\":[43]},{\"id\":43,\"callFrame\":{\"functionName\":\"InjectedScript.RemoteObject\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":123,\"columnNumber\":76},\"hitCount\":0,\"children\":[44,45,47]},{\"id\":44,\"callFrame\":{\"functionName\":\"_isHTMLAllCollection\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":89,\"columnNumber\":82},\"hitCount\":1,\"positionTicks\":[{\"line\":90,\"ticks\":1}]},{\"id\":45,\"callFrame\":{\"functionName\":\"_describe\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":95,\"columnNumber\":46},\"hitCount\":0,\"children\":[46]},{\"id\":46,\"callFrame\":{\"functionName\":\"_subtype\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":90,\"columnNumber\":94},\"hitCount\":1,\"positionTicks\":[{\"line\":91,\"ticks\":1}]},{\"id\":47,\"callFrame\":{\"functionName\":\"_generatePreview\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":149,\"columnNumber\":73},\"hitCount\":0,\"children\":[48,49]},{\"id\":48,\"callFrame\":{\"functionName\":\"_createEmptyPreview\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":147,\"columnNumber\":42},\"hitCount\":1,\"positionTicks\":[{\"line\":148,\"ticks\":1}]},{\"id\":49,\"callFrame\":{\"functionName\":\"_propertyDescriptors\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":46,\"columnNumber\":119},\"hitCount\":0,\"children\":[50]},{\"id\":50,\"callFrame\":{\"functionName\":\"process\",\"scriptId\":\"108\",\"url\":\"\",\"lineNumber\":47,\"columnNumber\":171},\"hitCount\":1,\"positionTicks\":[{\"line\":55,\"ticks\":1}]},{\"id\":51,\"callFrame\":{\"functionName\":\"setTimeout\",\"scriptId\":\"0\",\"url\":\"\",\"lineNumber\":-1,\"columnNumber\":-1},\"hitCount\":1,\"positionTicks\":[{\"line\":167,\"ticks\":1}]}],\"startTime\":152092173304,\"endTime\":152094741329,\"samples\":[],\"timeDeltas\":[]}}}"
  var profMsg = JSON.parse(profMsgTxt);
  processProfilingMessage(profMsg);
  lo(JSON.stringify(profMsg));
}
