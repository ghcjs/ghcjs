#ifndef ghcjs_HOST_OS
#include "../include_native/ghcplatform.h"
#else

#ifndef __GHCPLATFORM_H__
#define __GHCPLATFORM_H__

#define BuildPlatform_TYPE  ghcjs
#define HostPlatform_TYPE   ghcjs

#define ghcjs_BUILD  1
#define ghcjs_HOST  1

#define ghcjs_BUILD_ARCH  1
#define ghcjs_HOST_ARCH  1
#define BUILD_ARCH  "ghcjs"
#define HOST_ARCH  "ghcjs"

#define ghcjs_BUILD_OS  1
#define ghcjs_HOST_OS  1
#define BUILD_OS  "ghcjs"
#define HOST_OS  "ghcjs"

#define ghcjs_BUILD_VENDOR  1
#define ghcjs_HOST_VENDOR  1
#define BUILD_VENDOR  "ghcjs"
#define HOST_VENDOR  "ghcjs"

/* These TARGET macros are for backwards compatibility... DO NOT USE! */
#define TargetPlatform_TYPE ghcjs
#define ghcjs_TARGET  1
#define ghcjs_TARGET_ARCH  1
#define TARGET_ARCH  "ghcjs"
#define ghcjs_TARGET_OS  1
#define TARGET_OS  "ghcjs"
#define ghcjs_TARGET_VENDOR  1

#endif /* __GHCPLATFORM_H__ */

#endif
