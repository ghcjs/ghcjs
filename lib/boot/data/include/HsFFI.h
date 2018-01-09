#ifndef ghcjs_HOST_OS
#include "../include_native/HsFFI.h"
#else

/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000
 *
 * A mapping for Haskell types to C types, including the corresponding bounds.
 * Intended to be used in conjuction with the FFI.
 *
 * WARNING: Keep this file and StgTypes.h in synch!
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSFFI_H
#define HSFFI_H

#ifdef __cplusplus
extern "C" {
#endif

/* get types from GHC's runtime system */
#include "ghcconfig.h"
#include "stg/Types.h"

/* set limits for integral types (static config for GHCJS) */
#define __INT8_MIN		(-128)
#define __INT16_MIN		(-32767-1)
#define __INT32_MIN		(-2147483647-1)
#define __INT64_MIN		(-9223372036854775807LL-1)
#define __INT8_MAX		(127)
#define __INT16_MAX		(32767)
#define __INT32_MAX		(2147483647)
#define __INT64_MAX		(9223372036854775807LL)
#define __UINT8_MAX		(255U)
#define __UINT16_MAX		(65535U)
#define __UINT32_MAX		(4294967295U)
#define __UINT64_MAX		(18446744073709551615ULL)

typedef StgChar			HsChar;
typedef StgInt			HsInt;
typedef StgInt8			HsInt8;
typedef StgInt16		HsInt16;
typedef StgInt32		HsInt32;
typedef StgInt64		HsInt64;
typedef StgWord                 HsWord;
typedef StgWord8		HsWord8;
typedef StgWord16		HsWord16;
typedef StgWord32		HsWord32;
typedef StgWord64		HsWord64;
typedef StgFloat		HsFloat;
typedef StgDouble		HsDouble;
typedef StgInt			HsBool;
typedef void*			HsPtr;          /* this should better match StgAddr */
typedef void			(*HsFunPtr)(void); /* this should better match StgAddr */
typedef void*			HsStablePtr;

/* this should correspond to the type of StgChar in StgTypes.h */
#define HS_CHAR_MIN		0
#define HS_CHAR_MAX		0x10FFFF

#define HS_BOOL_FALSE           0
#define HS_BOOL_TRUE            1

#define HS_BOOL_MIN             HS_BOOL_FALSE
#define HS_BOOL_MAX             HS_BOOL_TRUE

#define HS_INT_MIN		__INT32_MIN
#define HS_INT_MAX		__INT32_MAX
#define HS_WORD_MAX		__UINT32_MAX

#define HS_INT8_MIN		__INT8_MIN
#define HS_INT8_MAX		__INT8_MAX
#define HS_INT16_MIN		__INT16_MIN
#define HS_INT16_MAX		__INT16_MAX
#define HS_INT32_MIN		__INT32_MIN
#define HS_INT32_MAX		__INT32_MAX
#define HS_INT64_MIN		__INT64_MIN
#define HS_INT64_MAX		__INT64_MAX
#define HS_WORD8_MAX		__UINT8_MAX
#define HS_WORD16_MAX		__UINT16_MAX
#define HS_WORD32_MAX		__UINT32_MAX
#define HS_WORD64_MAX		__UINT64_MAX

#define HS_FLOAT_RADIX		FLT_RADIX
#define HS_FLOAT_ROUNDS		FLT_ROUNDS
#define HS_FLOAT_EPSILON	FLT_EPSILON
#define HS_FLOAT_DIG		FLT_DIG
#define HS_FLOAT_MANT_DIG	FLT_MANT_DIG
#define HS_FLOAT_MIN		FLT_MIN
#define HS_FLOAT_MIN_EXP	FLT_MIN_EXP
#define HS_FLOAT_MIN_10_EXP	FLT_MIN_10_EXP
#define HS_FLOAT_MAX		FLT_MAX
#define HS_FLOAT_MAX_EXP	FLT_MAX_EXP
#define HS_FLOAT_MAX_10_EXP	FLT_MAX_10_EXP

#define HS_DOUBLE_RADIX		DBL_RADIX
#define HS_DOUBLE_ROUNDS	DBL_ROUNDS
#define HS_DOUBLE_EPSILON	DBL_EPSILON
#define HS_DOUBLE_DIG		DBL_DIG
#define HS_DOUBLE_MANT_DIG	DBL_MANT_DIG
#define HS_DOUBLE_MIN		DBL_MIN
#define HS_DOUBLE_MIN_EXP	DBL_MIN_EXP
#define HS_DOUBLE_MIN_10_EXP	DBL_MIN_10_EXP
#define HS_DOUBLE_MAX		DBL_MAX
#define HS_DOUBLE_MAX_EXP	DBL_MAX_EXP
#define HS_DOUBLE_MAX_10_EXP	DBL_MAX_10_EXP

#ifdef __cplusplus
}
#endif

#endif /* HSFFI_H */

#endif

