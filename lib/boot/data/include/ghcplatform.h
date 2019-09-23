#ifndef __GHCPLATFORM_H__
#define __GHCPLATFORM_H__

#define BuildPlatform_TYPE  x86_64_apple_darwin
#define HostPlatform_TYPE   asmjs_unknown_none

#define x86_64_apple_darwin_BUILD  1
#define asmjs_unknown_none_HOST  1

#define x86_64_BUILD_ARCH  1
#define asmjs_HOST_ARCH  1
#define BUILD_ARCH  "x86_64"
#define HOST_ARCH  "asmjs"

#define darwin_BUILD_OS  1
#define none_HOST_OS  1
#define BUILD_OS  "darwin"
#define HOST_OS  "none"

#define apple_BUILD_VENDOR  1
#define unknown_HOST_VENDOR  1
#define BUILD_VENDOR  "apple"
#define HOST_VENDOR  "unknown"

/* These TARGET macros are for backwards compatibility... DO NOT USE! */
#define TargetPlatform_TYPE asmjs_unknown_none
#define asmjs_unknown_none_TARGET  1
#define asmjs_TARGET_ARCH  1
#define TARGET_ARCH  "asmjs"
#define none_TARGET_OS  1
#define TARGET_OS  "none"
#define unknown_TARGET_VENDOR  1

#define UnregisterisedCompiler 1

#endif /* __GHCPLATFORM_H__ */

