#ifdef __GHCJS__
#include <stdio.h>
#include <stdlib.h>

#include <ghcjs.h>

void defaultJavaScriptHandler(const char* js, int safety, const char* types, void* result, ...) {
  fprintf(stderr, "GHCJS PANIC: No handler for JavaScript foreign imports installed. Import pattern:\n%s\n", js);
  fflush(stderr);
  exit(75);
}

javaScriptHandler _javaScriptHandler = defaultJavaScriptHandler;

javaScriptHandler getJavaScriptHandler() {
  return _javaScriptHandler;
}

void setJavaScriptHandler(javaScriptHandler h) {
  _javaScriptHandler = h;
}

#endif

