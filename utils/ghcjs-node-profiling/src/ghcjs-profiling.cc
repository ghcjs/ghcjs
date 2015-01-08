#include <node.h>
#include <stdio.h>

using namespace v8;

static void** StackStart = 0;
static void** StackEnd   = 0;

static void*  Callbacks[16];
static void** StackSlots[128];

const char* LogFile = "v8.log.ghcjs";

void CreateLog() {
  FILE *file = fopen(LogFile, "w");
  if(file) fclose(file);
}

FILE* OpenLog() {
  return fopen(LogFile, "a");
}

void CbX(int n, const FunctionCallbackInfo<Value>& args) {
  int i;
  int k;
  void** p;
  void** ss = StackStart;
  StackEnd = (void**)(&i);
  Isolate* isolate = Isolate::GetCurrent();
  HandleScope scope(isolate);
  if(n > 0) {
    const unsigned argc = 2;
    Local<Value> argv[argc] = { args[0],  args[1] };
    Local<Array> a          = Local<Array>::Cast(args[1]);
    Local<Function> cb      = Local<Function>::Cast(a->Get(n-1));
    cb->Call(isolate->GetCurrentContext()->Global(), argc, argv);
  } else {
    for(i=0;i<128;i++) StackSlots[i] = 0;
    for(i=0;i<16;i++) {
      p = StackEnd;
      k = 0;
      while(p < StackStart && k < 8) {
        if(*p == Callbacks[i]) {
          StackSlots[8*i+k] = p;
          k++;
        }
        p++;
      }
    }
    Local<Function> ncb = Local<Function>::Cast(args[0]);
    ncb->Call(isolate->GetCurrentContext()->Global(), 0, 0);
  }
  StackStart = ss;
}

void Cb1 (const FunctionCallbackInfo<Value>& args) { CbX(0,  args); }
void Cb2 (const FunctionCallbackInfo<Value>& args) { CbX(1,  args); }
void Cb3 (const FunctionCallbackInfo<Value>& args) { CbX(2,  args); }
void Cb4 (const FunctionCallbackInfo<Value>& args) { CbX(3,  args); }
void Cb5 (const FunctionCallbackInfo<Value>& args) { CbX(4,  args); }
void Cb6 (const FunctionCallbackInfo<Value>& args) { CbX(5,  args); }
void Cb7 (const FunctionCallbackInfo<Value>& args) { CbX(6,  args); }
void Cb8 (const FunctionCallbackInfo<Value>& args) { CbX(7,  args); }
void Cb9 (const FunctionCallbackInfo<Value>& args) { CbX(8,  args); }
void Cb10(const FunctionCallbackInfo<Value>& args) { CbX(9,  args); }
void Cb11(const FunctionCallbackInfo<Value>& args) { CbX(10, args); }
void Cb12(const FunctionCallbackInfo<Value>& args) { CbX(11, args); }
void Cb13(const FunctionCallbackInfo<Value>& args) { CbX(12, args); }
void Cb14(const FunctionCallbackInfo<Value>& args) { CbX(13, args); }
void Cb15(const FunctionCallbackInfo<Value>& args) { CbX(14, args); }
void Cb16(const FunctionCallbackInfo<Value>& args) { CbX(15, args); }

void RunCC(const FunctionCallbackInfo<Value>& args) {
  int x;
  TryCatch tc;
  StackStart = (void**)&x;
  CbX(16, args);
  StackStart = 0;
  if(tc.HasCaught()) tc.ReThrow();
}

void InitCC(const FunctionCallbackInfo<Value>& args) {
  int i;
  Callbacks[0]  = (void*)(&Cb1);  Callbacks[1]  = (void*)(&Cb2);
  Callbacks[2]  = (void*)(&Cb3);  Callbacks[3]  = (void*)(&Cb4);
  Callbacks[4]  = (void*)(&Cb5);  Callbacks[5]  = (void*)(&Cb6);
  Callbacks[6]  = (void*)(&Cb7);  Callbacks[7]  = (void*)(&Cb8);
  Callbacks[8]  = (void*)(&Cb9);  Callbacks[9]  = (void*)(&Cb10);
  Callbacks[10] = (void*)(&Cb11); Callbacks[11] = (void*)(&Cb12);
  Callbacks[12] = (void*)(&Cb13); Callbacks[13] = (void*)(&Cb14);
  Callbacks[14] = (void*)(&Cb15); Callbacks[15] = (void*)(&Cb16);
  CreateLog();
  FILE* log = OpenLog();
  if(!log) return;
  fprintf(log, "cc-callbacks,%#lx", (unsigned long)(&RunCC));
  for(i=0;i<16;i++) fprintf(log, ",%#lx", (unsigned long)Callbacks[i]);
  fprintf(log, "\n");
  fclose(log);
}

void AddCC(const FunctionCallbackInfo<Value>& args) {
  unsigned char cname[256], cfile[256];
  Isolate* isolate = Isolate::GetCurrent();
  HandleScope scope(isolate);
  Local<Integer> n    = Local<Integer>::Cast(args[0]);
  Local<String>  name = Local<String>::Cast(args[1]);
  Local<String>  file = Local<String>::Cast(args[2]);
  Local<Integer> line = Local<Integer>::Cast(args[3]);
  Local<Integer> col  = Local<Integer>::Cast(args[4]);
  name->WriteOneByte(cname, 0, 256);
  file->WriteOneByte(cfile, 0, 256);
  FILE* log = OpenLog();
  if(!log) return;
  fprintf( log, "cc-creation,%d,\"%s\",\"%s\",%d,%d\n"
	 , (int)n->Value(), cname, cfile, (int)line->Value(), (int)col->Value());
  fclose(log);
}

void AddCCS(const FunctionCallbackInfo<Value>& args) {
  uint32_t i;
  Isolate* isolate = Isolate::GetCurrent();
  HandleScope scope(isolate);
  Local<Integer> n   = Local<Integer>::Cast(args[0]);
  if(!args[1]->IsArray()) {
    isolate->ThrowException(Exception::TypeError(
      String::NewFromUtf8(isolate, "Second argument must be an array")));
    return;
  }
  Local<Array>   ccs = Local<Array>::Cast(args[1]);
  for(i=0;i<ccs->Length();i++) {
    if(!ccs->Get(i)->IsNumber()) {
      isolate->ThrowException(Exception::TypeError(
        String::NewFromUtf8(isolate, "Array elements must be numbers")));
      return;
    }
  }
  FILE* log = OpenLog();
  if(!log) return;
  fprintf(log, "ccs-creation,%d", (int)n->Value());
  for(i=0;i<ccs->Length();i++) {
    fprintf(log, ",%d", (int)ccs->Get(i)->NumberValue());
  }
  fprintf(log, "\n");
  fclose(log);
}

void SetCCS(const FunctionCallbackInfo<Value>& args) {
  int i, j, n;
  Isolate* isolate = Isolate::GetCurrent();
  HandleScope scope(isolate);
  if(StackStart == 0) return;
  Local<NumberObject> jn = Local<NumberObject>::Cast(args[0]);
  n = (int)jn->NumberValue();
  for(i=0;i<16;i++) {
    for(j=0;j<8;j++) {
      if(StackSlots[8*i+j]) {
        *(StackSlots[8*i+j]) = Callbacks[n&15];
      } else break;
    }
    n >>= 4;
  }
}

void Init(Handle<Object> exports, Handle<Object> module) {
  NODE_SET_METHOD(exports, "call1" , Cb1);
  NODE_SET_METHOD(exports, "call2" , Cb2);
  NODE_SET_METHOD(exports, "call3" , Cb3);
  NODE_SET_METHOD(exports, "call4" , Cb4);
  NODE_SET_METHOD(exports, "call5" , Cb5);
  NODE_SET_METHOD(exports, "call6" , Cb6);
  NODE_SET_METHOD(exports, "call7" , Cb7);
  NODE_SET_METHOD(exports, "call8" , Cb8);
  NODE_SET_METHOD(exports, "call9" , Cb9);
  NODE_SET_METHOD(exports, "call10", Cb10);
  NODE_SET_METHOD(exports, "call11", Cb11);
  NODE_SET_METHOD(exports, "call12", Cb12);
  NODE_SET_METHOD(exports, "call13", Cb13);
  NODE_SET_METHOD(exports, "call14", Cb14);
  NODE_SET_METHOD(exports, "call15", Cb15);
  NODE_SET_METHOD(exports, "call16", Cb16);
  NODE_SET_METHOD(exports, "initcc", InitCC);
  NODE_SET_METHOD(exports, "addcc",  AddCC);
  NODE_SET_METHOD(exports, "addccs", AddCCS);
  NODE_SET_METHOD(exports, "setccs", SetCCS);
  NODE_SET_METHOD(exports, "runcc",  RunCC);
}

NODE_MODULE(callback, Init)

