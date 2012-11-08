/*
   tests for the garbage collector
   requires Node and Mocha

   ghcjs -dump-rts
   mocha gc.js
*/

var assert = require('assert');
var h      = require('./dummy.gen2.js'); // require('./rts.gen2.js');

// load an array into registers
var glbl = this;
function loadRegs(regs) {
  for(var i=0;i<regs.length;i++) {
    h.setReg(i+1,regs[i]);
  }
}

function mkFunClosure(f, name, arity, followable) {
  f.gi   = [1];  // heap object size 1, no free vars
  f.gtag = 1;
  f.n    = name;
  f.a    = arity;
  f.t    = h.FUN_CLOSURE;
  f.s    = [];
  f.gai  = followable;
}

function mkHeapObj(f, name, ty, size, followable) {
  f.gi   = [size].concat(followable);
  f.gtag = mkTag(size, followable);
  f.n    = name;
  f.t    = ty;
}

function mkTag(size, followable) {
  var tag = size;
  for(var i=0;i<followable.length;i++) {
    var offset = followable[i];
    if(offset >= 25) throw "invalid offset for gc tag";
    tag = tag | (1 << (offset+7));
  }
  return tag;
}

// function with no followable arguments
var fun1 = function () { };
mkFunClosure(fun1, "fun1", 1, []);

// function with followable arguments in r1 and r2
var fun2 = function() { };
mkFunClosure(fun2, "fun2", 2, [1,2]);

// thunk with two followable free vars
var thunk20 = function() { };
mkHeapObj(thunk20,"thunk20",h.THUNK_CLOSURE,3,[1,2]);

// thunk with two unfollowable free vars
var thunk02 = function() { };
mkHeapObj(thunk02,"thunk02",h.THUNK_CLOSURE,3,[]);

// thunk with one unfollowable free var
var thunk01 = function() { };
mkHeapObj(thunk01,"thunk01",h.THUNK_CLOSURE,2,[]);

// thunk with one followable (second), one unfollowable (first) free var
var thunk11 = function() { };
mkHeapObj(thunk11,"thunk11",h.THUNK_CLOSURE,3,[2]);

// thunk with one followable free var
var thunk10 = function() { };
mkHeapObj(thunk10,"thunk10",h.THUNK_CLOSURE,2,[1]);

// indirect object, first argument is always followable
var ind1 = function() { };
mkHeapObj(ind1,"ind1",h.IND_CLOSURE,2,[1]);

// shallow copy o1 and o2, o2 takes priority
function extend(o1, o2) {
  var o = {};
  var i;
  for(i in o1) { o[i] = o1[i]; }
  for(i in o2) { o[i] = o2[i]; }
  return o;
}

function showHeapElem(x) {
  if(typeof x == "function") {
    if(x.i && x.i.length && x.i.length >= 2) {
      return ("fun: " + x.i[1]);
    } else {
      return ("error: non-entry fun");
    }
  } else {
    return (""+x);
  }
}

describe('sortBoth', function() {
  it("should sort arrays correctly", function() {
    var lengths = [1,2,3,5,10,15,24,25,26,35,50,100,1000,9001,10000];
    var f = function(x) { return (Math.sin(x)*1000)|0; }
    for(var i=0;i<lengths.length;i++) {
      var l = lengths[i];
      var test1 = [];
      var test2 = [];
      for(var j=0;j<l;j++) {
        var r = (10000*Math.random())|0;
        test1.push(r);
        test2.push(f(r));
      }
      h.quicksortBoth(test1,test2);
      var t = test1[0];
      for(var j=0;j<l;j++) {
        assert.ok(t <= test1[j], "first array sorted");
        assert.ok(test2[j] === f(test1[j]), "second array moved with first");
        t = test1[j];
      }
    }
  });
});

describe('gc', function() {
  var d        = 10;  // start of dynamic heap
  // true if arrays are equal on elements [0..max-1]
  var assertEqualUpTo = function(a1, a2, max, name) {
     for(var i = 0; i < max; i++) {
       assert.strictEqual(a1[i], a2[i], name + "[" + i + "]: " + showHeapElem(a1[i]) + " !== " + showHeapElem(a2[i]));
     }
     return true;
  }
  // makes a heap from the static and dynamic parts
  var mkHeap = function(stat, dyn) {
    var r = stat.slice(0);
    while(r.length < d) r.push(0);
    return r.concat(dyn);
  }

  // run full or oncremental gc and check dynamic heap (optionally stack and registers) for equality
  var gcCheck = function(incremental, c) { // stat,dyn1,dyn2,next,stack,regs,hpe) {
    var heapBefore = mkHeap(c.stat, c.dyn);    // before
    var heapAfter  = mkHeap(c.stat, c.expDyn); // expected result
    var stackBefore = c.stack.slice(0);
//    h.dumpHeapFromTo(heapBefore, 0, heapBefore.length);
    loadRegs(c.regs);
    h.dumpHeapFromTo(heapBefore, 0, heapAfter.length);
//    h.dumpHeap(heapBefore);
    var hp = h.gc(heapBefore, 
                  heapBefore.length,
                  stackBefore,
                  stackBefore.length-1,
                  c.next,
                  d,
                  c.old,
                  c.forward,
                  c.forwardStatic,
                  incremental);
//    console.log("heap pointer: " + hp);
//    h.dumpHeap(heapBefore);
    assertEqualUpTo(heapBefore, heapAfter, hp, "heap");
    assert.equal(heapAfter.length, hp);
    if(c.expStack) assertEqualUpTo(c.expStack,stackBefore,stackBefore.length, "stack");
    if(c.expRegs) {
      for(var i=0;i<c.expRegs.length;i++) {
        assert.equal(c.expRegs[i], h.getReg(i+1), "register r"+(i+1)+ ": " + c.expRegs[i] + " !== " + h.getReg(i+1));
      }
    }
  }

  var t = { stat:  [fun1, fun2]
          , regs:  [d, d+3, d+6]
          , stack: [fun1]
          , next: fun1
          , forward: []
          , forwardStatic: []
          , old: d               // everything before this is old generation
          };

  describe("[full gc]", function() {
    it("should remove unreachable objects", function() {
      gcCheck(false, extend(t, { dyn:    [thunk02,-1,-1]
                               , expDyn: []
      }));
    });

    it("should keep objects referred to by stack", function() {
       gcCheck(false, extend(t, { dyn:    [thunk02,-1,-1]
                                , expDyn: [thunk02,-1,-1]
                                , stack:  [d,thunk10,fun1]
       }));
    });

    it("should move reachable objects to the start of heap", function() {
       gcCheck(false, extend(t, { dyn:     [thunk01,-1,thunk02,-2,-3]
                                , expDyn:  [thunk02,-2,-3]
                                , stack:   [d+2,thunk10,fun1]
       }));

       gcCheck(false, extend(t, { dyn:     [thunk01,-1,thunk02,-2,-3,thunk02,-4,-5,thunk01,-6]
                                , expDyn:  [thunk02,-2,-3,thunk01,-6]
                                , stack:   [d+8,d+2,thunk20,fun1]
       }));

    });

    it("should follow arguments", function() {

       gcCheck(false, extend(t, { dyn:     [thunk02,-1,-2,thunk02,-3,-4,thunk02,-5,-6]
                                , expDyn:  []
                                , next:    fun1
       }));

       gcCheck(false, extend(t, { dyn:     [thunk02,-1,-2,thunk02,-3,-4,thunk02,-5,-6]
                                , expDyn:  [thunk02,-1,-2,thunk02,-3,-4]
                                , next:    fun2
       }));

    });

    it("should update pointers", function() {
       gcCheck(false, extend(t, { dyn:      [thunk01,-1,thunk02,-2,-3]
                                , expDyn:   [thunk02,-2,-3]
                                , stack:    [d+2,thunk10,fun1]
                                , expStack: [d,thunk10,fun1]
       }));

       gcCheck(false, extend(t, { dyn:      [thunk01,-1,thunk02,-2,-3,thunk11,-5,d+2,thunk11,-7, d+5]
                                , expDyn:   [thunk02,-2,-3,thunk11,-5,d,thunk11,-7,d+3]
                                , stack:    [d+8,thunk10,fun1]
                                , expStack: [d+6,thunk10,fun1]
       }));
    });

    it("should update function arguments", function() {
       gcCheck(false, extend(t, { dyn:      [thunk01,-1,thunk02,-2,-3]
                                , expDyn:   [thunk02,-2,-3]
                                , stack:    [d+2,thunk10,fun1]
                                , expStack: [d,thunk10,fun1]
                                , next:     fun2
                                , regs:     [d+2,d+2]
                                , expRegs:  [d, d]
       }));
    });
    it("should follow indirections", function() {
       gcCheck(false, extend(t, { dyn:      [ind1,d+5,thunk02,-1,-2,thunk11,-3,d+2]
                                , expDyn:   [thunk02,-1,-2,thunk11,-3,d]
                                , stack:    [d,thunk10,fun1]
                                , expStack: [d+3,thunk10,fun1]
                                , next:     fun2
                                , regs:     [d,d]
                                , expRegs:  [d+3,d+3]
      }));
    });

    it("should update indirect arguments", function() {
       gcCheck(false, extend(t, { dyn:      [ind1,d+5,thunk20,d+5,d,thunk11,-3,d+2]
                                , expDyn:   [thunk20,d+3,d+3,thunk11,-3,d]
                                , stack:    [d+2,thunk10,fun1]
                                , expStack: [d,thunk10,fun1]
                                , next:     fun2
                                , regs:     [d,d]
                                , expRegs:  [d+3,d+3]
      }));
    });
  });

  describe("[incremental gc]", function() {
    it("should only remove new generation objects", function() {
      gcCheck(true, extend(t, { dyn:      [thunk02,-1,-2,thunk02,-3,-4,thunk02,-5,-6]
                              , expDyn:   [thunk02,-1,-2,thunk02,-5,-6]
                              , stack:    [d+6,thunk10,fun1]
                              , expStack: [d+3,thunk10,fun1]
                              , old:      d+3
      }));
    });

    it("should follow and update forward pointers in the new generation", function() {
      gcCheck(true, extend(t, { dyn:      [ind1,d+8,thunk02,-1,-2,thunk02,-3,-4,thunk02,-5,-6]
                              , expDyn:   [ind1,d+5,thunk02,-1,-2,thunk02,-5,-6]
                              , stack:    [fun1]
                              , expStack: [fun1]
                              , old:      d+5
                              , forward:  [d]
      }));
    });

    it("should keep forward-pointing objects that don't point to new gen intact", function() {
      gcCheck(true, extend(t, { dyn:      [ind1,d+10,ind1,d+4,thunk02,-1,-2,thunk02,-3,-4,thunk02,-5,-6]
                              , expDyn:   [ind1,d+7,ind1,d+4,thunk02,-1,-2,thunk02,-5,-6]
                              , stack:    [fun1]
                              , expStack: [fun1]
                              , old:      d+7
                              , forward:  [d,d+2]
      }));
    });
  });
});

