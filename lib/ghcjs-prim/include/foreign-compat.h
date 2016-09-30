#ifdef FOREIGN_IMPORT_STUBS
#define FOREIGN_IMPORT(safety,name,type,str) \
  ;name :: type \
  ;name = error "'name' (a foreign import) is not defined. This is a stub enabled by the foreign-import-stubs flag."
#else
#define FOREIGN_IMPORT(safety,name,type,str) \
  ;foreign import javascript safety str name :: type
#endif
