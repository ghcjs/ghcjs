/*
  Native extension for GHCJS profiling

  Due to tail call optimization, profile data from Haskell call stacks is
  not very useful. GHC uses Cost Centre Stacks (CCS) as an additional mechanism
  for attributing costs to code locations.

  Unfortunately JavaScript profilers have no knowledge of Cost Centre Stacks.

  The v8 profiler periodically takes a sample of the current instruction
  pointer and call stack (by default every 1000 microseconds).

  In order to get useful Haskell profiling data, we need the active cost
  centre stack for each sample.

  We could use the event log to send a message every time the CCS changes,
  but this would add considerable overhead, since CCS changes are very
  frequent.

  Instead we use v8's ReturnAddressLocationResolver to encode the CCS
  directly on the call stack.

  Example call stack:

    someFunction
    h$_prof_ccs_a_0
    h$_prof_ccs_a_1
    h$_prof_ccs_a_2
    h$mainLoop
    h$someCompiledCode

  The ReturnAddressLocationResolver recognizes the h$_prof_ccs_a_x frames
  and returns h$_prof_ccs_b_y frames, with the value y encoding 10 bits
  of the cost centre stack.

    someFunction
    h$_prof_ccs_b_u
    h$_prof_ccs_b_v
    h$_prof_ccs_b_w
    h$mainLoop
    h$someCompiledCode

  For example if the current CCS id is 337345233, then

    ccsid = 337345233
    u = (ccsid >>> 20) & 1023 = 321
    v = (ccsid >>> 10) & 1023 = 734
    w = ccsid & 1023          = 721

  So the recorded call stack sample looks like

    someFunction
    h$_prof_ccs_b_321
    h$_prof_ccs_b_734
    h$_prof_ccs_b_721
    h$mainLoop
    h$someCompiledCode

  The actual data on the stack never change.

  A script in the devtools retrieves all the cost centre information from
  the Haskell runtime and expands the h$_prof_ccs_b_x triples to the full
  cost centre stack.

  Note:

  The functions need to be optimized for this to work. The JavaScript part
  of this extension uses %OptimizeFunctionOnNextCall(f) for this, which
  requires the --allow-natives-syntax v8 startup option.

 */
#include <map>

// use the old-fashioned Node API
#include <node.h>
#include <stdio.h>

using namespace v8;

const int nCostCentreFuns     = 1024;
const int nCostCentreFunsBits = 10;
const int nFunctions          = nCostCentreFuns + 3;
const int ccsReturnAddress1   = nCostCentreFuns;
const int ccsReturnAddress2   = nCostCentreFuns + 1;
const int ccsReturnAddress3   = nCostCentreFuns + 2;

static uint32_t currentCCS    = 0;

/*
  a code location of one of the functions we're interested in

    start == 0 && len == 0 <=> the item location is unknown

  cc_code_locations contains an entry for each known item

  item numbering:
    [0,1023]    -> h$_prof_ccs_b_
    [1024,1026] -> h$_prof_ccs_a_
 */
typedef struct code_t {
  uintptr_t start;
  size_t len;
} code_t;


static code_t    cc_code[nFunctions];
static uintptr_t cc_return[nFunctions];
static std::map<uintptr_t, int> cc_code_locations; // start -> item

bool is_in_code(uintptr_t return_addr, int item) {
  code_t &c = cc_code[item];
  return return_addr >= c.start && return_addr < c.start + c.len;
}

int code_offset(uintptr_t return_addr, int item) {
  return return_addr - cc_code[item].start;
}

static uintptr_t address_loc_resolver(uintptr_t return_addr_location) {
  uintptr_t ra = *((uintptr_t*)return_addr_location);
  int item = -1;
  uintptr_t offset = 0;
  if(is_in_code(ra, ccsReturnAddress1)) {
    item = currentCCS & (nCostCentreFuns-1);
    offset = code_offset(ra, ccsReturnAddress1);
  } else if(is_in_code(ra, ccsReturnAddress2)) {
    item = (currentCCS >> nCostCentreFunsBits) & (nCostCentreFuns-1);
    offset = code_offset(ra, ccsReturnAddress2);
  } else if(is_in_code(ra, ccsReturnAddress3)) {
    item = (currentCCS >> (2 * nCostCentreFunsBits)) & (nCostCentreFuns-1);
    offset = code_offset(ra, ccsReturnAddress3);
  }
  if(item != -1 && cc_code[item].start != 0) {
    cc_return[item] = cc_code[item].start + offset;
    return (uintptr_t)(&cc_return[item]);
  } else {
    return return_addr_location;
  }
}

int function_item(const char* name, int len) {
  if(len < 27 || strncmp(name, "LazyCompile:*h$_prof_ccs_", 25) != 0) {
    return -1;
  }
  if(name[26] != '_') return -1;
  char ver = name[25];
  if(ver != 'a' && ver != 'b') return -1;
  name += 27;
  len -= 27;
  if(len == 0 || !isdigit(name[0])) return -1;
  int val = 0;
  for(int i=0;i<len;i++) {
    char c = name[i];
    if(!isdigit(c)) break;
    val = val * 10 + (int)c - 48;
  }
  if(ver == 'b' && val >= 0 && val <= 1023) {
    return val;
  } else if(ver == 'a' && val >= 0 && val <= 2) {
    return val + 1024;
  } else {
    return -1;
  }
}

static void jit_code_handler(const JitCodeEvent* event) {
  char* name;
  int item;
  uintptr_t cstart = (uintptr_t)event->code_start;
  std::map<uintptr_t, int>::iterator it;
  switch(event->type) {
    case JitCodeEvent::CODE_ADDED:
      name = new char[event->name.len+1];
      name[event->name.len] = 0;
      strncpy(name, event->name.str, event->name.len);
      fprintf( stderr
             , "jit: code added at %p size %lu, name: %s\n"
             , event->code_start
             , event->code_len
             , name);
      delete[] name;
      item = function_item(event->name.str, event->name.len);
      if(item != -1) {
        it = cc_code_locations.find(cstart);
        // code for a different function in same location? shouldn't happen
        if(it != cc_code_locations.end() && it->second != item) {
          cc_code[it->second].start = 0;
          cc_code[it->second].len = 0;

        }
        cc_code[item].start = cstart;
        cc_code[item].len = event->code_len;
        cc_code_locations[cstart] = item;
      }
      break;
    case JitCodeEvent::CODE_MOVED:
      fprintf( stderr
             , "jit: code moved at %p size %lu to: %p\n"
             , event->code_start
             , event->code_len
             , event->new_code_start
             );
      it = cc_code_locations.find(cstart);
      if(it != cc_code_locations.end()) {
        item = it->second;
        cstart = (uintptr_t)event->new_code_start;
        cc_code[item].start = cstart;
        cc_code[item].len   = event->code_len;
        cc_code_locations.erase(it);
        cc_code_locations[cstart] = item;
      }
      break;
    case JitCodeEvent::CODE_REMOVED:
      fprintf( stderr
             , "jit: code removed from %p size %lu\n"
             , event->code_start
             , event->code_len
             );
      it = cc_code_locations.find(cstart);
      if(it != cc_code_locations.end()) {
        item = it->second;
        cc_code[item].start = 0;
        cc_code[item].len = 0;
        cc_code_locations.erase(it);
      }
      break;
    case JitCodeEvent::CODE_ADD_LINE_POS_INFO:
    case JitCodeEvent::CODE_START_LINE_INFO_RECORDING:
    case JitCodeEvent::CODE_END_LINE_INFO_RECORDING:
    default:
      ; // no need to handle these
  }
}

void get_ccs_buffer(const FunctionCallbackInfo<Value>& args) {
  Isolate* isolate = args.GetIsolate();
  args.GetReturnValue().Set(
    ArrayBuffer::New(isolate, &currentCCS, 4)
  );
}

void Init(Handle<Object> exports, Handle<Object> module) {
  // set all tracked code locations to unknown
  for(int i=0;i<nFunctions;i++) {
    cc_code[i].start = 0;
    cc_code[i].len   = 0;
  }

  // install handlers
  v8::Isolate* isolate = v8::Isolate::GetCurrent();
  V8::SetReturnAddressLocationResolver(address_loc_resolver);
  isolate->SetJitCodeEventHandler(kJitCodeEventDefault, jit_code_handler);

  // expose get_ccs_buffer
  NODE_SET_METHOD(exports, "get_ccs_buffer", get_ccs_buffer);
}

NODE_MODULE(NODE_GYP_MODULE_NAME, Init)
