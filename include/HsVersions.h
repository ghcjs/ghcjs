#ifndef HSVERSIONS_H
#define HSVERSIONS_H

/* some definitions from HsVersions.h so that we can compile files from the GHC source tree */

/* Global variables may not work in other Haskell implementations,
 * but we need them currently! so the conditional on GLASGOW won't do. */
#if defined(__GLASGOW_HASKELL__) || !defined(__GLASGOW_HASKELL__)
#define GLOBAL_VAR(name,value,ty)  \
{-# NOINLINE name #-};             \
name :: IORef (ty);                \
name = Util.global (value);

#define GLOBAL_VAR_M(name,value,ty) \
{-# NOINLINE name #-};              \
name :: IORef (ty);                 \
name = Util.globalM (value);
#endif

#define ASSERT(e)      if debugIsOn && not (e) then (assertPanic __FILE__ __LINE__) else
#define ASSERT2(e,msg) if debugIsOn && not (e) then (assertPprPanic __FILE__ __LINE__ (msg)) else
#define WARN( e, msg ) (warnPprTrace (e) __FILE__ __LINE__ (msg)) $
#define MASSERT(e)      ASSERT(e) return ()
#define MASSERT2(e,msg) ASSERT2(e,msg) return ()
#define ASSERTM(e)      do { bool <- e; MASSERT(bool) }
#define ASSERTM2(e,msg) do { bool <- e; MASSERT2(bool,msg) }
#define WARNM2(e,msg)   do { bool <- e; WARN(bool, msg) return () }

#endif
