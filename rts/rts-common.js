function $hs_mkBool(b) {
    return b ? $$GHCziTypes_True:$$GHCziTypes_False;
};
function $hs_tagToEnumzh(t) {
    n = WORD_SIZE_IN_BITS==32 ? t : t.toNumber();
    return $d(n+1, []);
};
function $hs_dataToTagzh(d) {
    return $hs_int(d.g-1);
};
function $hs_mulIntMayOflozh(a, b) {
    if(WORD_SIZE_IN_BITS==32) {
        var x = a * b;
        return x===(x|0)?0:1;
    }
    else {
        return goog.math.Long.fromBits(0,
            (a.isNegative()?a.negate():a).getHighBits() |
            (b.isNegative()?b.negate():b).getHighBits());
    }
};
function $hs_ztzh(a, b) {
    // Safe 32bit multiply 64 bit multiply is done inline
    return goog.math.Long.fromInt(a).multiply(goog.math.Long.fromInt(b)).getLowBits();
};
function $hs_addIntCzh(a, b) {
    if(WORD_SIZE_IN_BITS==32) {
        var x = a + b;
        return [x|0, x===(x|0)?0:1];
    }
    else {
        var x = goog.math.Integer.fromBits([a.getLowBits(), a.getHighBits()])
            .add(goog.math.Integer.fromBits([b.getLowBits(), b.getHighBits()]));
        return [goog.math.Long.fromBits(x.getBits(0), x.getBits(1)),
            x.sign_===x.getBits(2)?0:1];
    }
};
function $hs_subIntCzh(a, b) {
    if(WORD_SIZE_IN_BITS==32) {
        var x = a - b;
        return [x|0, x===(x|0)?0:1];
    }
    else {
        var x = goog.math.Integer.fromBits([a.getLowBits(), a.getHighBits()])
            .subtract(goog.math.Integer.fromBits([b.getLowBits(), b.getHighBits()]));
        return [goog.math.Long.fromBits(x.getBits(0), x.getBits(1)),
            x.sign_===x.getBits(2)?0:1];
    }
};
function $hs_ordzh(a) {
    if(WORD_SIZE_IN_BITS==32) {
        return a.charCodeAt();
    }
    else {
        return goog.math.Long.fromNumber(a.charCodeAt());
    }
};
function $hs_chrzh(a) {
    if(WORD_SIZE_IN_BITS==32) {
        return String.fromCharCode(a);
    }
    else {
        return String.fromCharCode(a.toNumber());
    }
};
var $hs_popCntTab =
   [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8];
function $hs_popCnt8(a) {
    return $hs_popCntTab[a & 0xFF];
};
function $hs_popCnt8zh(a) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_popCnt8(a);
    }
    else {
        return goog.math.Long.fromNumber(
            $hs_popCnt8(a.getLowBits()));
    }
};
function $hs_popCnt16zh(a) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_popCnt8(a) + $hs_popCnt8(a>>>8);
    }
    else {
        return goog.math.Long.fromNumber(
            $hs_popCnt8(a.getLowBits())+$hs_popCnt8(a.getLowBits()>>>8));
    }
};
function $hs_popCnt32zh(a) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_popCnt8(a) + $hs_popCnt8(a>>>8) + $hs_popCnt8(a>>>16) + $hs_popCnt8(a>>>24);
    }
    else {
        var l = a.getLowBits();
        return goog.math.Long.fromNumber(
            $hs_popCnt8(l)+$hs_popCnt8(l>>>8) + $hs_popCnt8(l>>>16) + $hs_popCnt8(l>>>24));
    }
};
function $hs_popCnt64zh(a) {
    var l = a.getLowBits();
    var h = a.getHighBits();
    var result =
        $hs_popCnt8(l)+$hs_popCnt8(l>>>8) + $hs_popCnt8(l>>>16) + $hs_popCnt8(l>>>24) +
        $hs_popCnt8(h)+$hs_popCnt8(h>>>8) + $hs_popCnt8(h>>>16) + $hs_popCnt8(h>>>24);
    if(WORD_SIZE_IN_BITS==32) {
        return result;
    }
    else {
        return goog.math.Long.fromNumber(result);
    }
};
var $hs_popCntzh = WORD_SIZE_IN_BITS==32 ? $hs_popCnt32zh : $hs_popCnt64zh;
function $hs_expDoublezh(a) {
    return Math.exp(a);
};
function $hs_logDoublezh(a) {
    return Math.log(a);
};
function $hs_sqrtDoublezh(a) {
    return Math.sqrt(a);
};
function $hs_sinDoublezh(a) {
    return Math.sin(a);
};
function $hs_cosDoublezh(a) {
    return Math.cos(a);
};
function $hs_tanDoublezh(a) {
    return Math.tan(a);
};
function $hs_asinDoublezh(a) {
    return Math.asin(a);
};
function $hs_acosDoublezh(a) {
    return Math.acos(a);
};
function $hs_atanDoublezh(a) {
    return Math.atan(a);
};
function $hs_sinhDoublezh(a) {
    return (Math.exp(a) - Math.exp(-a)) / 2;
};
function $hs_coshDoublezh(a) {
    return (Math.exp(a) + Math.exp(-a)) / 2;
};
function $hs_tanhDoublezh(a) {
    var expA = Math.exp(a);
    var expNegA = Math.exp(-a);
    return (expA - expNegA) / (expA + expNegA);
};
function $hs_ztztzhzh(a, b) {
    return Math.pow(a,b);
};
function $hs_double2Intzh(f) {
    return $hs_int(f|0);
};
function $hs_float2Intzh(f) {
    return $hs_int(f|0);
};
function $hs_decodeDoublezu2Intzh(x) {
    if( x < 0 ) {
        var result = $hs_decodeDoublezu2Intzh(-x);
        return [-1, result[1], result[2], result[3]];
    }
    var negExponent = 52-Math.floor(Math.log(x) * 1.4426950408889634); // 1/log(2)
    var mantissa = goog.math.Long.fromNumber(x * Math.pow(2, negExponent));
    return [1, mantissa.getHighBits(), mantissa.getLowBits(), -negExponent];
};
function $hs_expFloatzh(a) {
    return Math.exp(a);
};
function $hs_logFloatzh(a) {
    return Math.log(a);
};
function $hs_sqrtFloatzh(a) {
    return Math.sqrt(a);
};
function $hs_sinFloatzh(a) {
    return Math.sin(a);
};
function $hs_cosFloatzh(a) {
    return Math.cos(a);
};
function $hs_tanFloatzh(a) {
    return Math.tan(a);
};
function $hs_asinFloatzh(a) {
    return Math.asin(a);
};
function $hs_acosFloatzh(a) {
    return Math.acos(a);
};
function $hs_atanFloatzh(a) {
    return Math.atan(a);
};
function $hs_sinhFloatzh(a) {
    return (Math.exp(a) - Math.exp(-a)) / 2;
};
function $hs_coshFloatzh(a) {
    return (Math.exp(a) + Math.exp(-a)) / 2;
};
function $hs_tanhFloatzh(a) {
    var expA = Math.exp(a);
    var expNegA = Math.exp(-a);
    return (expA - expNegA) / (expA + expNegA);
};
function $hs_powerFloatzh(a, b) {
    return Math.pow(a, b);
};
function $hs_decodeFloatzuIntzh(x) {
    // This probably gives unexpected results.
    // In particular we need to check...
    //   * NaN
    //   * Infinity
    if( x < 0 ) {
        var result = $hs_decodeFloatzuIntzh(-x);
        return [-result[0], result[1]];
    }
    var negExponent = 23-Math.floor(Math.log(x) * 1.4426950408889634); // 1/log(2)
    return [$hs_int((x * Math.pow(2, negExponent))|0), $hs_int(-negExponent)];
};
function isDoubleNegativeZero(a) {
    return $hs_int((a==-0.0)?1:0);
};
function isDoubleNaN(a) {
    return $hs_int((isNaN(a))?1:0);
};
function isDoubleInfinite(a) {
    return $hs_int((isFinite(a)||isNaN(a))?0:1);
};
function isFloatNegativeZero(a) { // fixme switch to proper 32 bit floats
    return $hs_int((a==-0.0)?1:0);
};
function isFloatNaN(a) {
    return $hs_int((isNaN(a))?1:0);
};
function isFloatInfinite(a) {
    return $hs_int((isFinite(a)||isNaN(a))?0:1);
};
function integer_cbits_encodeDouble(s, bits, e) {
    var g = $hs_gmpToGoog(s, bits);
    return Math.pow(2,e)*g.toNumber();
};
function __int_encodeDouble(b, e) {
    return Math.pow(2,e) * b;
};
function rintDouble(a) {
    return Math.round(a);
};
function rintFloat(a) {
    return Math.round(a);
};
function $hs_newMutVarzh(a, s) {
    return [s, {value : a}];
};
function $hs_readMutVarzh(a, s) {
    return [s, a.value];
};
function $hs_writeMutVarzh(a, b, s) {
    a.value = b;
    return s;
};
function $hs_sameMutVarzh(a, b) {
    return a === b;
};
function $hs_newArrayzh(n, a, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var result = [];
    for (var x = 0; x != n; x++)
      result[x] = a;
    return [s, result];
};
function $hs_sameMutableArrayzh(a, b) {
    return a === b;
};
function $hs_readArrayzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [s, a[n]];
};
function $hs_writeArrayzh(a, n, b, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    a[n] = b;
    return s;
};
function $hs_sizeofArrayzh(a, s) {
    return [s, $hs_int(a.length)];
};
function $hs_sizeofMutableArrayzh(a, s) {
    return [s, $hs_int(a.length)];
};
function $hs_indexArrayzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [a[n]]; // Unboxed singleton
};
function $hs_unsafeFreezzeArrayzh(a, s) {
    return [s, a];
};
function $hs_unsafeThawArrayzh(a, s) {
    return [s, a];
};
function $hs_int(i) {
    return WORD_SIZE_IN_BITS == 32 ? i : goog.math.Long.fromNumber(i);
};
if(WORD_SIZE_IN_BITS==32) {
    // ByteArray support
    var $hs_ptrBase = 0;

    /**
     * @param {!number} n
     * @param {Object=} s
     */
    function $hs_newByteArrayzh(n, s) {
        var result = [new ArrayBuffer(n), 0, $hs_ptrBase];
        result[0].ptrs=[];
        $hs_ptrBase += n;
        return [s, result];
    };
    /**
     * @param {!number} n
     * @param {Object=} s
     */
    function $hs_newPinnedByteArrayzh(n, s) {
        var result = [new ArrayBuffer(n), 0, $hs_ptrBase];
        result[0].ptrs=[];
        $hs_ptrBase += n;
        return [s, result];
    };
    /**
     * @param {!number} n
     * @param {!number} k
     * @param {Object=} s
     */
    function $hs_newAlignedPinnedByteArrayzh(n, k, s) {
        $hs_ptrBase += $hs_ptrBase%k;
        var result = [new ArrayBuffer(n), 0, $hs_ptrBase];
        result[0].ptrs=[];
        $hs_ptrBase += n;
        return [s, result];
    };
}
if (WORD_SIZE_IN_BITS==64) {
    // ByteArray support
    var $hs_ptrBase = goog.math.Long.ZERO;

    /**
     * @param {!goog.math.Long} n
     * @param {Object=} s
     */
    function $hs_newByteArrayzh(n, s) {
        var result = [new ArrayBuffer(n.toInt()), 0, $hs_ptrBase];
        result[0].ptrs=[];
        $hs_ptrBase = $hs_ptrBase.add(n);
        return [s, result];
    };
    /**
     * @param {!goog.math.Long} n
     * @param {Object=} s
     */
    function $hs_newPinnedByteArrayzh(n, s) {
        var result = [new ArrayBuffer(n.toInt()), 0, $hs_ptrBase];
        result[0].ptrs=[];
        $hs_ptrBase = $hs_ptrBase.add(n);
        return [s, result];
    };
    /**
     * @param {!goog.math.Long} n
     * @param {!goog.math.Long} k
     * @param {Object=} s
     */
    function $hs_newAlignedPinnedByteArrayzh(n, k, s) {
        $hs_ptrBase = $hs_ptrBase.add(goog.math.Long.fromInt($hs_ptrBase.toInt() % k.toInt()));
        var result = [new ArrayBuffer(n.toNumber()), 0, $hs_ptrBase];
        result[0].ptrs=[];
        $hs_ptrBase = $hs_ptrBase.add(n);
        return [s, result];
    };
}

function $hs_byteArrayContentszh(a) {
    return a;
};
function $hs_sameMutableByteArrayzh(a, b) {
    return a[2] === b[2];
};
function $hs_unsafeFreezzeByteArrayzh(a, s) {
    return [s, a];
};
function $hs_sizeofByteArrayzh(a) {
    return new Uint8Array(a[0]).length;
};
function $hs_sizeofMutableByteArrayzh(a) {
    return new Uint8Array(a[0]).length;
};
function $hs_indexCharArrayzh(a, n) {
    return String.fromCharCode(new Uint8Array(a[0])[n]);
};
function $hs_indexWideCharArrayzh(a, n) {
    return String.fromCharCode(new Uint32Array(a[0])[n]);
};
// --- goog.math.Integer and (# Int#, ByteArray# #) mapping
//
// Unfortunately goog.math.Integer.bits_ are the bits of a signed in.
// goog.math.Integer.sign_ is the value of the rest of the infinite list.
//
// We want (s, bits) where bits is the ByteArray of the bits of the
// absolute value. And s is
//    | n == 0 -> 0
//    | n > 0  -> length bits
//    | n < 0  -> -(length bits)
//
// For performance we will store the goog.math.Integer as the ByteArray
// and spin up a cached positive version to work out the correct bits.
//
// When constructing the goog.math.Integer in most cases ByteArray
// will be the desired number.
function $hs_fastNegate(a) {
    if(a.negateCache_ === undefined) {
        a.negateCache_ = a.negate();
        a.negateCache_.negateCache_ = a;
    }
    return a.negateCache_;
};
function $hs_absolute(a) {
    if(a.isNegative()) {
        return $hs_fastNegate(a);
    }
    return a;
};
function $hs_googToGMP(a) {
    var bits = $hs_absolute(a).bits_;
    var s = bits.length;
    while(s !== 0 && bits[s-1] === 0)
        s--;
    s = a.isNegative() ? -s : s;
    return [WORD_SIZE_IN_BITS==32 ? s : goog.math.Long.fromNumber(s), a];
};
function $hs_gmpToGoog(s,bits) {
    s = WORD_SIZE_IN_BITS==32 ? s : s.toNumber();

    // If s is 0 then the number we want is 0
    if(s===0) {
        return goog.math.Integer.ZERO;
    }

    // We are going to need to know long the bits array should be
    var len = s < 0 ? -s : s;

    // Try to avoid making a new integer if we can
    if(bits instanceof goog.math.Integer) {
        var current = $hs_googToGMP(bits);
        // Make sure we do not have too many bits
        if(len >= current[0]) {
            if((s<0) === bits.isNegative()) {
                // Both same sign so just return bits
                return bits;
            }
            else {
                // Only the sign is changed
                return $hs_fastNegate(bits);
            }
        }
    }

    // Ok lets build a new one
    var newBits = [];
    for(var n = 0; n !== len; n++) {
        newBits[n] = $hs_indexIntArrayzh(bits, n);
    }
    var i = new goog.math.Integer(newBits, 0);
    return s < 0 ? $hs_fastNegate(i) : i;
};
function $hs_indexIntArrayzh(a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        if(a instanceof goog.math.Integer) {
            return $hs_absolute(a).getBits(n);
        }
        return new Int32Array(a[0])[n];
    }
    else {
        var n2 = n.toNumber()<<1;
        if(a instanceof goog.math.Integer) {
            var positive = $hs_absolute(a);
            return goog.math.Long.fromBits(positive.getBits(n2), positive.getBits(n2+1));
        }
        var x = new Int32Array(a[0]);
        return goog.math.Long.fromBits(x[n2], x[n2+1]);
    }
};
function $hs_indexWordArrayzh(a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        // fixme there should be something better than checking this manually
        if(a instanceof goog.math.Integer) return $hs_absolute(a).getBits(n);
        else return new Uint32Array(a[0])[n];
    }
    else {
        var x = new Int32Array(a[0], n.toNumber()<<3);
        return goog.math.Long.fromBits(x[0], x[1]);
    }
};
function $hs_checkPointer(p, n) {
    if(!((p === null && n === 0) || p === n))
        throw "Pointer Error";
};
function $hs_indexAddrArrayzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var res = a[0].ptrs[n];
    if(HS_DEBUG)
      $hs_checkPointer(res, new Uint32Array(a[0])[n]);
    return res;
};
function $hs_indexFloatArrayzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return new Float32Array(a[0])[n];
};
function $hs_indexDoubleArrayzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return new Float64Array(a[0])[n];
};
function $hs_indexStablePtrArrayzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var res = a[0].ptrs[n];
    if(HS_DEBUG)
      $hs_checkPointer(res, new Uint32Array(a[0])[n]);
    return res;
};
function $hs_indexInt8Arrayzh(a, n) {
    if(WORD_SIZE_IN_BITS == 32) {
        return new Int8Array(a[0])[n];
    } else {
        return goog.math.Long.fromNumber(new Int8Array(a[0])[n.toNumber()]);
    }
};
function $hs_indexInt16Arrayzh(a, n) {
    if(WORD_SIZE_IN_BITS == 32) {
        return new Int16Array(a[0])[n];
    } else {
        return goog.math.Long.fromNumber(new Int16Array(a[0])[n.toNumber()]);
    }
};
function $hs_indexInt32Arrayzh(a, n) {
    if(WORD_SIZE_IN_BITS == 32) {
        return new Int32Array(a[0])[n];
    } else {
        return goog.math.Long.fromNumber(new Int32Array(a[0])[n.toNumber()]);
    }
};
function $hs_indexInt64Arrayzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], n<<3);
    return goog.math.Long.fromBits(x[0], x[1]);
};
function $hs_indexWord8Arrayzh(a, n) {
    if(WORD_SIZE_IN_BITS == 32) {
        return new Uint8Array(a[0])[n];
    } else {
        return goog.math.Long.fromBits(new Uint8Array(a[0])[n.toNumber()], 0);
    }
};
function $hs_indexWord16Arrayzh(a, n) {
    if(WORD_SIZE_IN_BITS == 32) {
        return new Uint16Array(a[0])[n];
    } else {
        return goog.math.Long.fromBits(new Uint16Array(a[0])[n.toNumber()], 0);
    }
};
function $hs_indexWord32Arrayzh(a, n) {
    if(WORD_SIZE_IN_BITS == 32) {
        return new Uint32Array(a[0])[n];
    } else {
        return goog.math.Long.fromBits(new Uint32Array(a[0])[n.toNumber()], 0);
    }
};
function $hs_indexWord64Arrayzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], n<<3);
    return goog.math.Long.fromBits(x[0], x[1]);
};
function $hs_readCharArrayzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [s, String.fromCharCode(new Uint8Array(a[0])[n])];
};
function $hs_readWideCharArrayzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [s, String.fromCharCode(new Uint32Array(a[0])[n])];
};
function $hs_readIntArrayzh(a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Int32Array(a[0])[n]];
    }
    else {
        var x = new Int32Array(a[0], n.toNumber()<<3);
        return [s, goog.math.Long.fromBits(x[0], x[1])];
    }
};
function $hs_readWordArrayzh(a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Uint32Array(a[0])[n]];
    }
    else {
        var x = new Int32Array(a[0], n.toNumber()<<3);
        return [s, goog.math.Long.fromBits(x[0], x[1])];
    }
};
function $hs_readAddrArrayzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var res = a[0].ptrs[n];
    if(HS_DEBUG)
      $hs_checkPointer(res, new Uint32Array(a[0])[n]);
    return [s, res];
};
function $hs_readFloatArrayzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [s, new Float32Array(a[0])[n]];
};
function $hs_readDoubleArrayzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [s, new Float64Array(a[0])[n]];
};
function $hs_readStablePtrArrayzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var res = a[0].ptrs[n];
    if(HS_DEBUG)
      $hs_checkPointer(res, new Uint32Array(a[0])[n]);
    return [s, res];
};
function $hs_readInt8Arrayzh(a, n, s) {
    if(WORD_SIZE_IN_BITS == 32) {
        return [s, new Int8Array(a[0])[n]];
    } else {
        return [s, goog.math.Long.fromNumber(new Int8Array(a[0])[n.toNumber()])];
    }
};
function $hs_readInt16Arrayzh(a, n, s) {
    if(WORD_SIZE_IN_BITS == 32) {
        return [s, new Int16Array(a[0])[n]];
    } else {
        return [s, goog.math.Long.fromNumber(new Int16Array(a[0])[n.toNumber()])];
    }
};
function $hs_readInt32Arrayzh(a, n, s) {
    if(WORD_SIZE_IN_BITS == 32) {
        return [s, new Int32Array(a[0])[n]];
    } else {
        return [s, goog.math.Long.fromNumber(new Int32Array(a[0])[n.toNumber()])];
    }
};
function $hs_readInt64Arrayzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], n<<3);
    return [s, goog.math.Long.fromBits(x[0], x[1])];
};
function $hs_readWord8Arrayzh(a, n, s) {
    if(WORD_SIZE_IN_BITS == 32) {
        return [s, new Uint8Array(a[0])[n]];
    } else {
        return [s, goog.math.Long.fromBits(new Uint8Array(a[0])[n.toNumber()], 0)];
    }
};
function $hs_readWord16Arrayzh(a, n, s) {
    if(WORD_SIZE_IN_BITS == 32) {
        return [s, new Uint16Array(a[0])[n]];
    } else {
        return [s, goog.math.Long.fromBits(new Uint16Array(a[0])[n.toNumber()], 0)];
    }
};
function $hs_readWord32Arrayzh(a, n, s) {
    if(WORD_SIZE_IN_BITS == 32) {
        return [s, new Uint32Array(a[0])[n]];
    } else {
        return [s, goog.math.Long.fromBits(new Uint32Array(a[0])[n.toNumber()], 0)];
    }
};
function $hs_readWord64Arrayzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], n<<3);
    return [s, goog.math.Long.fromBits(x[0], x[1])];
};
function $hs_writeCharArrayzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    new Uint8Array(a[0])[n] = v.charCodeAt();
    return s;
};
function $hs_writeWideCharArrayzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    new Uint32Array(a[0])[n] = v.charCodeAt();
    return s;
};
function $hs_writeIntArrayzh(a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Int32Array(a[0])[n] = v;
        return s;
    }
    else {
        var x = new Int32Array(a[0], n.toNumber()<<3);
        x[0] = v.getLowBits();
        x[1] = v.getHighBits();
        return s;
    }
};
function $hs_writeWordArrayzh(a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Uint32Array(a[0])[n] = v;
        return s;
    }
    else {
        var x = new Int32Array(a[0], n.toNumber()<<3);
        x[0] = v.getLowBits();
        x[1] = v.getHighBits();
        return s;
    }
};
function $hs_writeAddrArrayzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    a[0].ptrs[n] = v;
    new Uint32Array(a[0])[n] = (v === null ? 0 : v[2]);
    return s;
};
function $hs_writeFloatArrayzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    new Float32Array(a[0])[n] = v;
    return s;
};
function $hs_writeDoubleArrayzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    new Float64Array(a[0])[n] = v;
    return s;
};
function $hs_writeStablePtrArrayzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    a[0].ptrs[n] = v;
    new Uint32Array(a[0])[n] = (v === null ? 0 : v[2]);
    return s;
};
function $hs_writeInt8Arrayzh(a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Int8Array(a[0])[n] = v;
    }
    else {
        new Int8Array(a[0])[n.toNumber()] = v.getLowBits();
    }
    return s;
};
function $hs_writeInt16Arrayzh(a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Int16Array(a[0])[n] = v;
    }
    else {
        new In16Array(a[0])[n.toNumber()] = v.getLowBits();
    }
    return s;
};
function $hs_writeInt32Arrayzh(a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Int32Array(a[0])[n] = v;
    }
    else {
        new Int32Array(a[0])[n.toNumber()] = v.getLowBits();
    }
    return s;
};
function $hs_writeInt64Arrayzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], n<<3);
    x[0] = v.getLowBits();
    x[1] = v.getHighBits();
    return s;
};
function $hs_writeWord8Arrayzh(a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Uint8Array(a[0])[n] = v;
    }
    else {
        new Uint8Array(a[0])[n.toNumber()] = v.getLowBits();
    }
    return s;
};
function $hs_writeWord16Arrayzh(a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Uint16Array(a[0])[n] = v;
    }
    else {
        new Uint16Array(a[0])[n.toNumber()] = v.getLowBits();
    }
    return s;
};
function $hs_writeWord32Arrayzh(a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Uint32Array(a[0])[n] = v;
    }
    else {
        new Uint32Array(a[0])[n.toNumber()] = v.getLowBits();
    }
    return s;
};
function $hs_writeWord64Arrayzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], n<<3);
    x[0] = v.getLowBits();
    x[1] = v.getHighBits();
    return s;
};
function $hs_copyByteArrayzh(src, soff, dest, doff, count, s) {
    var srcarray = new Uint8Array(src[0]);
    var destarray = new Uint8Array(dest[0]);
    while(count != 0) {
        destarray[doff] = srcarray[soff];
        soff++;
        doff++;
        count--;
    }
    return s;
};
function $hs_copyMutableByteArrayzh(src, soff, dest, doff, count, s) {
    var srcarray = new Uint8Array(src[0]);
    var destarray = new Uint8Array(dest[0]);
    while(count != 0) {
        destarray[doff] = srcarray[soff];
        soff++;
        doff++;
        count--;
    }
    return s;
};
function $hs_plusAddrzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    if(typeof(a) === 'string')
        return [a, n, n];
    else
        return [a[0],a[1]+n,a[2]+n];
};
function $hs_minusAddrzh(a, b) {
    return $hs_int(a[1]-b[1]);
};
function $hs_remAddrzh(a, b) {
    return a[1]%b;
};
function $hs_gtAddrzh(a, b) {
    return $hs_mkBool(
         (a===null&&b===null)?false:
        ((a===null&&b!==null)?false:
        ((a!==null&&b===null)?true:(a[2]>b[2]))));
};
function $hs_geAddrzh(a, b) {
    return $hs_mkBool(
         (a===null&&b===null)?true:
        ((a===null&&b!==null)?false:
        ((a!==null&&b===null)?true:(a[2]>=b[2]))));
};
function $hs_eqAddrzh(a, b) {
    return $hs_mkBool(
         (a===null&&b===null)?true:
        ((a===null&&b!==null)?false:
        ((a!==null&&b===null)?false:(a[2]===b[2]))));
};
function $hs_neAddrzh(a, b) {
    return $hs_mkBool(
         (a===null&&b===null)?false:
        ((a===null&&b!==null)?true:
        ((a!==null&&b===null)?true:(a[2]!==b[2]))));
};
function $hs_ltAddrzh(a, b) {
    return $hs_mkBool(
         (a===null&&b===null)?false:
        ((a===null&&b!==null)?true:
        ((a!==null&&b===null)?false:(a[2]<b[2]))));
};
function $hs_leAddrzh(a, b) {
    return $hs_mkBool(
         (a===null&&b===null)?true:
        ((a===null&&b!==null)?true:
        ((a!==null&&b===null)?false:(a[2]<=b[2]))));
};
function $hs_indexCharOffAddrzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    if(typeof(a) === 'string')
        return n==a.length?'\x00':a.charAt(n);
    else if(typeof(a[0]) === 'string')
        return n==a[0].length?'\x00':a[0].charAt(a[1]+n);
    else
        return String.fromCharCode(new Uint8Array(a[0],a[1]+n)[0]);
};
function $hs_indexWideCharOffAddrzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return String.fromCharCode(new Uint32Array(a[0],a[1]+(n<<2))[0]);
};
function $hs_indexIntOffAddrzh(a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Int32Array(a[0],a[1]+(n<<2))[0];
    }
    else {
        var x = new Int32Array(a[0], a[1] + (n.toNumber()<<3));
        return goog.math.Long.fromBits(x[0], x[1]);
    }
};
function $hs_indexWordOffAddrzh(a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Uint32Array(a[0],a[1]+(n<<2))[0];
    }
    else {
        var x = new Int32Array(a[0], a[1] + (n.toNumber()<<3));
        return goog.math.Long.fromBits(x[0], x[1]);
    }
};
function $hs_indexAddrOffAddrzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var res = a[0].ptrs[a[1]+(n<<2)];
    if(HS_DEBUG)
      $hs_checkPointer(res, new Uint32Array(a[0],a[1]+(n<<2))[0]);
    return res;
};
function $hs_indexFloatOffAddrzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return new Float32Array(a[0],a[1]+(n<<2))[0];
};
function $hs_indexDoubleOffAddrzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return new Float64Array(a[0],a[1]+(n<<3))[0];
};
function $hs_indexStablePtrOffAddrzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var res = a[0].ptrs[a[1]+(n<<2)];
    if(HS_DEBUG)
      $hs_checkPointer(res, new Uint32Array(a[0],a[1]+(n<<2))[0]);
    return res;
};
function $hs_indexInt8OffAddrzh(a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Int8Array(a[0],a[1]+n)[0];
    }
    else {
        return goog.math.Long.fromNumber(new Int8Array(a[0],a[1]+n.toNumber())[0]);
    }
};
function $hs_indexInt16OffAddrzh(a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Int16Array(a[0],a[1]+(n<<1))[0];
    }
    else {
        return goog.math.Long.fromNumber(new Int16Array(a[0],a[1]+(n.toNumber()<<1))[0]);
    }
};
function $hs_indexInt32OffAddrzh(a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Int32Array(a[0],a[1]+(n<<2))[0];
    }
    else {
        return goog.math.Long.fromNumber(new Int32Array(a[0],a[1]+(n.toNumber()<<2))[0]);
    }
};
function $hs_indexInt64OffAddrzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Uint32Array(a[0],a[1]+(n<<3));
    return goog.math.Long.fromBits(x[0], x[1]);
};
function $hs_indexWord8OffAddrzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    if(typeof(a) === 'string')
        var res = n==a.length?0:a.charCodeAt(n);
    else if(typeof(a[0]) === 'string')
        var res = n==a[0].length?0:a[0].charCodeAt(a[1]+n);
    else
        var res = new Uint8Array(a[0],a[1]+n)[0];
    return WORD_SIZE_IN_BITS==32 ? res : goog.math.Long.fromBits(res);
};
function $hs_indexWord16OffAddrzh(a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Uint16Array(a[0],a[1]+(n<<1))[0];
    }
    else {
        return goog.math.Long.fromBits(new Uint16Array(a[0],a[1]+(n.toNumber()<<1))[0], 0);
    }
};
function $hs_indexWord32OffAddrzh(a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Uint32Array(a[0],a[1]+(n<<2))[0];
    }
    else {
        return goog.math.Long.fromBits(new Uint32Array(a[0],a[1]+(n.toNumber()<<2))[0], 0);
    }
};
function $hs_indexWord64OffAddrzh(a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0],a[1]+(n<<3));
    return goog.math.Long.fromBits(x[0], x[1]);
};
function $hs_readCharOffAddrzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [s, String.fromCharCode(new Uint8Array(a[0],a[1]+n)[0])];
};
function $hs_readWideCharOffAddrzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [s, String.fromCharCode(new Uint32Array(a[0],a[1]+(n<<2))[0])];
};
function $hs_readIntOffAddrzh(a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Int32Array(a[0],a[1]+(n<<2))[0]];
    }
    else {
        var x = new Int32Array(a[0], a[1] + (n.toNumber()<<3));
        return [s, goog.math.Long.fromBits(x[0], x[1])];
    }
};
function $hs_readWordOffAddrzh(a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Uint32Array(a[0],a[1]+(n<<2))[0]];
    }
    else {
        var x = new Int32Array(a[0], a[1] + (n.toNumber()<<3));
        return [s, goog.math.Long.fromBits(x[0], x[1])];
    }
};
function $hs_readAddrOffAddrzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var res = a[0].ptrs[a[1]+(n<<2)];
    if(HS_DEBUG)
      $hs_checkPointer(res, new Uint32Array(a[0],a[1]+(n<<2))[0]);
    return [s, res];
};
function $hs_readFloatOffAddrzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [s, new Float32Array(a[0],a[1]+(n<<2))[0]];
};
function $hs_readDoubleOffAddrzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [s, new Float64Array(a[0],a[1]+(n<<3))[0]];
};
function $hs_readStablePtrOffAddrzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var res = a[0].ptrs[a[1]+(n<<2)];
    if(HS_DEBUG)
      $hs_checkPointer(res, new Uint32Array(a[0],a[1]+(n<<2))[0])
    return [s, res];
};
function $hs_readInt8OffAddrzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    if(typeof(a) === 'string')
        var res = n==a.length?0:a.charCodeAt(n);
    else if(typeof(a[0]) === 'string')
        var res = n==a[0].length?0:a[0].charCodeAt(a[1]+n);
    else
        var res = new Uint8Array(a[0],a[1]+n)[0];
    return [s, WORD_SIZE_IN_BITS==32 ? res : goog.math.Long.fromBits(res)];
};
function $hs_readInt16OffAddrzh(a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Int16Array(a[0],a[1]+(n<<1))[0]];
    }
    else {
        return [s, goog.math.Long.fromNumber(new Int16Array(a[0],a[1]+(n.toNumber()<<1))[0])];
    }
};
function $hs_readInt32OffAddrzh(a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Int32Array(a[0],a[1]+(n<<2))[0]];
    }
    else {
        return [s, goog.math.Long.fromNumber(new Int32Array(a[0],a[1]+(n.toNumber()<<2))[0])];
    }
};
function $hs_readInt64OffAddrzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Uint32Array(a[0],a[1]+(n<<3));
    return [s, goog.math.Long.fromBits(x[0], x[1])];
};
function $hs_readWord8OffAddrzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    if(typeof(a) === 'string')
        return [s, a.charCodeAt(n)];
    else
        return [s, goog.math.Long.fromBits(new Uint8Array(a[0],a[1]+n)[0], 0)];
};
function $hs_readWord16OffAddrzh(a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Uint16Array(a[0],a[1]+(n<<1))[0]];
    }
    else {
        return [s, goog.math.Long.fromBits(new Uint16Array(a[0],a[1]+(n.toNumber()<<1))[0], 0)];
    }
};
function $hs_readWord32OffAddrzh(a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Uint32Array(a[0],a[1]+(n<<2))[0]];
    }
    else {
        return [s, goog.math.Long.fromBits(new Uint32Array(a[0],a[1]+(n.toNumber()<<2))[0], 0)];
    }
};
function $hs_readWord64OffAddrzh(a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Uint32Array(a[0],a[1]+(n<<3));
    return [s, goog.math.Long.fromBits(x[0], x[1])];
};
function $hs_writeCharOffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    (new Uint8Array(a[0],a[1]+n))[0] = v.charCodeAt();
    return s;
};
function $hs_writeWideCharOffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    (new Uint32Array(a[0],a[1]+(n<<2)))[0] = v.charCodeAt();
    return s;
};
function $hs_writeIntOffAddrzh(a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        (new Int32Array(a[0],a[1]+(n<<2)))[0] = v;
        return s;
    }
    else {
        var x = new Int32Array(a[0], a[1]+(n.toNumber()<<3));
        x[0] = v.getLowBits();
        x[1] = v.getHighBits();
        return s;
    }
};
function $hs_writeWordOffAddrzh(a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        (new Uint32Array(a[0],a[1]+(n<<2)))[0] = v;
        return s;
    }
    else {
        var x = new Int32Array(a[0], a[1]+(n.toNumber()<<3));
        x[0] = v.getLowBits();
        x[1] = v.getHighBits();
        return s;
    }
};
function $hs_writeAddrOffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    a[0].ptrs[n] = v;
    (new Uint32Array(a[0],a[1]+(n<<2)))[0] = (v === null ? 0 : v[2]);
    return s;
};
function $hs_writeFloatOffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    (new Float32Array(a[0],a[1]+(n<<2)))[0] = v;
    return s;
};
function $hs_writeDoubleOffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    (new Float64Array(a[0],a[1]+(n<<3)))[0] = v;
    return s;
};
function $hs_writeStablePtrOffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    a[0].ptrs[n] = v;
    (new Uint32Array(a[0],a[1]+(n<<2)))[0] = (v === null ? 0 : v[2]);
    return s;
};
function $hs_writeInt8OffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    (new Int8Array(a[0],a[1]+n))[0] = WORD_SIZE_IN_BITS==32 ? v : v.getLowBits();
    return s;
};
function $hs_writeInt16OffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    (new Int16Array(a[0],a[1]+(n<<1)))[0] = WORD_SIZE_IN_BITS==32 ? v : v.getLowBits();
    return s;
};
function $hs_writeInt32OffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    (new Int32Array(a[0],a[1]+(n<<2)))[0] = WORD_SIZE_IN_BITS==32 ? v : v.getLowBits();
    return s;
};
function $hs_writeInt64OffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], a[1]+(n<<3));
    x[0] = v.getLowBits();
    x[1] = v.getHighBits();
    return s;
};
function $hs_writeWord8OffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    (new Uint8Array(a[0],a[1]+n))[0] = WORD_SIZE_IN_BITS==32 ? v : v.getLowBits();
    return s;
};
function $hs_writeWord16OffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    (new Uint16Array(a[0],a[1]+(n<<1)))[0] = WORD_SIZE_IN_BITS==32 ? v : v.getLowBits();
    return s;
};
function $hs_writeWord32OffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    (new Uint32Array(a[0],a[1]+(n<<2)))[0] = WORD_SIZE_IN_BITS==32 ? v : v.getLowBits();
    return s;
};
function $hs_writeWord64OffAddrzh(a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], a[1]+(n<<3));
    x[0] = v.getLowBits();
    x[1] = v.getHighBits();
    return s;
};
function $hs_logAny(c, str) {
    console.log(c + ": " + str);
};
function $hs_logInfo(str) {
    $hs_logAny("INFO", str);
};
function $hs_logError(str) {
    $hs_logAny("ERROR", str);
};
function $hs_logDebug(str) {
    $hs_logAny("DEBUG", str);
};

var $hs_logger = goog.debug.Logger.getLogger('hs');

function $hs_utf32(s) {
    var res = $hs_newByteArrayzh($hs_int((s.length<<2)+4))[1];
    var dest = new Uint32Array(res[0]);
    for(var i=0;i!=s.length;i++)
        dest[i]=s.charCodeAt(i);
    dest[i]=0;
    return res;
};
function $hs_fromUtf32(s) {
    var res = "";
    var src = new Uint32Array(s[0],s[1]);
    var len = src[src.length-1] === 0 ? src.length - 1 : src.length;
    for(var i=0;i!=len;i++)
        res=res+String.fromCharCode(src[i]);
    return res;
};
function $hs_ascii(s) {
    var res = $hs_newByteArrayzh($hs_int(s.length+1))[1];
    var dest = new Uint8Array(res[0]);
    for(var i=0;i!=s.length;i++)
        dest[i]=s.charCodeAt(i);
    dest[i]=0;
    return res;
};
function integer_cmm_cmpIntegerzh(sa, abits, sb, bbits) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_gmpToGoog(sa, abits).compare($hs_gmpToGoog(sb, bbits));
    }
    else {
        return goog.math.Long.fromNumber($hs_gmpToGoog(sa, abits).compare(
            $hs_gmpToGoog(sb, bbits)));
    }
};
function integer_cmm_cmpIntegerIntzh(sa, abits, b) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_gmpToGoog(sa, abits).compare(goog.math.Integer.fromInt(b));
    }
    else {
        return goog.math.Long.fromNumber($hs_gmpToGoog(sa, abits).compare(
            goog.math.Integer.fromBits([b.getLowBits(), b.getHighBits()])));
    }
};
function integer_cmm_plusIntegerzh(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).add($hs_gmpToGoog(sb, bbits)));
};
function integer_cmm_minusIntegerzh(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).subtract($hs_gmpToGoog(sb, bbits)));
};
function integer_cmm_timesIntegerzh(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).multiply($hs_gmpToGoog(sb, bbits)));
};
function integer_cmm_quotRemIntegerzh(sa, abits, sb, bbits) {
    var a = $hs_gmpToGoog(sa, abits);
    var b = $hs_gmpToGoog(sb, bbits)
    var q = a.divide(b);
    var r = a.subtract(q.multiply(b));
    return $hs_googToGMP(q).concat($hs_googToGMP(r));
};
function integer_cmm_quotIntegerzh(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).divide($hs_gmpToGoog(sb, bbits)));
};
function integer_cmm_remIntegerzh(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).modulo($hs_gmpToGoog(sb, bbits)));
};
function integer_cmm_divModIntegerzh(sa, abits, sb, bbits) {
    var a = $hs_gmpToGoog(sa, abits);
    var b = $hs_gmpToGoog(sb, bbits);
    var d = a.divide(b);
    var m = a.subtract(d.multiply(b));
    if(a.isNegative()!==b.isNegative() && !m.isZero()) {
        // Take one off d and add b onto m
        d = d.add(goog.math.Integer.fromInt(-1));
        m = m.add(b);
    }
    return $hs_googToGMP(d).concat($hs_googToGMP(m));
};
function integer_cmm_divExactIntegerzh(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).divide($hs_gmpToGoog(sb, bbits)));
};
function $hs_gcd(a, b) {
    var x = $hs_absolute(a);
    var y = $hs_absolute(b);
    var big, small;
    if(x.lessThan(y)) {
        small = x;
        big = y;
    }
    else {
        small = x;
        big = y;
    }
    while(!small.isZero()) {
        var q = big.divide(small);
        var r = big.subtract(q.multiply(small));
        big = small;
        small = r;
    }
    return $hs_googToGMP(big);
};
function integer_cmm_gcdIntegerzh(sa, abits, sb, bbits) {
    return $hs_gcd($hs_gmpToGoog(sa, abits), $hs_gmpToGoog(sb, bbits));
};
function integer_cmm_gcdIntegerIntzh(sa, abits, b) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_gcd($hs_gmpToGoog(sa, abits), goog.math.Integer.fromInt(b));
    }
    else {
        return $hs_gcd($hs_gmpToGoog(sa, abits),
            goog.math.Integer.fromBits([b.getLowBits(), b.getHighBits()]));
    }
};
function integer_cmm_gcdIntzh(a, b) {
    if(WORD_SIZE_IN_BITS==32) {
        var x = a<0 ? -a : a;
        var y = b<0 ? -b : b;
        var big, small;
        if(x<y) {
            small = x;
            big = y;
        }
        else {
            small = x;
            big = y;
        }
        while(small!==0) {
            var r = big % small;
            big = small;
            small = r;
        }
        return big;
    }
    else {
        var x = a.isNegative() ? a.negate() : a;
        var y = b.isNegative() ? b.negate() : b;
        var big, small;
        if(x.lessThan(y)) {
            small = x;
            big = y;
        }
        else {
            small = x;
            big = y;
        }
        while(!small.isZero()) {
            var q = big.div(small);
            var r = big.subtract(q.multiply(small));
            big = small;
            small = r;
        }
        return big;
    }
};
function integer_cmm_decodeDoublezh(x) {
    if( x < 0 ) {
        var result = integer_cmm_decodeDoublezh(-x);
        return [result[0], -result[1], result[2]];
    }
    var negExponent = 52-Math.floor(Math.log(x) * 1.4426950408889634); // 1/log(2)
    return [$hs_int(-negExponent)].concat($hs_googToGMP(
        goog.math.Integer.fromNumber(x * Math.pow(2, negExponent))));
};
function integer_cmm_int2Integerzh(i) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_googToGMP(goog.math.Integer.fromInt(i));
    }
    else {
        return $hs_googToGMP(
            goog.math.Integer.fromBits([i.getLowBits(), i.getHighBits()]));
    }
};
function integer_cmm_word2Integerzh(i) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_googToGMP(goog.math.Integer.fromBits(i<0?[0,i]:[i]));
    }
    else {
        return $hs_googToGMP(goog.math.Integer.fromBits(
            i.isNegative()?[0, i.getLowBits(), i.getHighBits()]
                          :[   i.getLowBits(), i.getHighBits()]));
    }
};
function integer_cmm_andIntegerzh(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).and($hs_gmpToGoog(sb, bbits)));
};
function integer_cmm_orIntegerzh(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).or($hs_gmpToGoog(sb, bbits)));
};
function integer_cmm_xorIntegerzh(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).xor($hs_gmpToGoog(sb, bbits)));
};
function integer_cmm_mul2ExpIntegerzh(sa, abits, b) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).shiftLeft(b));
};
function integer_cmm_fdivQ2ExpIntegerzh(sa, abits, b) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).shiftRight(b));
};
function integer_cmm_complementIntegerzh(sa, abits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).not());
};
function integer_cmm_int64ToIntegerzh(a) {
    return $hs_googToGMP(goog.math.Integer.fromBits([a.getLowBits(), a.getHighBits()]));
};
function integer_cmm_word64ToIntegerzh(a) {
    return $hs_googToGMP(goog.math.Integer.fromBits(
            a.isNegative()?[0, a.getLowBits(), a.getHighBits()]
                          :[   a.getLowBits(), a.getHighBits()]));
};
function hs_integerToInt64(as, abits) {
    var a = $hs_gmpToGoog(as, abits);
    return goog.math.Long.fromBits(a.getBits(0), a.getBits(1));
};
function hs_integerToWord64(as, abits) {
    var a = $hs_gmpToGoog(as, abits);
    return goog.math.Long.fromBits(a.getBits(0), a.getBits(1));
};
/**
 * @param {!Array.<Object>}     dest
 * @param {!number}             doff
 * @param {!Array.<Object>}     src
 * @param {!number}             soff
 * @param {!number}             count
 */
function _hs_text_memcpy(dest, doff, src, soff, count) {
    var srcarray = new Uint16Array(src[0],src[1]);
    var destarray = new Uint16Array(dest[0],dest[1]);
    while(count != 0) {
        destarray[doff] = srcarray[soff];
        soff++;
        doff++;
        count--;
    }
};
function _hs_text_memcmp(a, aoff, b, boff, count) {
    var aarray = new Uint16Array(a[0],a[1]);
    var barray = new Uint16Array(b[0],b[1]);
    while(count != 0) {
        if( aarray[aoff] < barray[boff] )
            return -1;
        if( aarray[aoff] > barray[boff] )
            return 1;
        aoff++;
        boff++;
        count--;
    }
    return 0;
};
/**
 * @define {number} size of Word and Int. If 64 we use goog.math.Long.
 */
var $hs_UTF8_ACCEPT = 0;
/**
 * @define {number} size of Word and Int. If 64 we use goog.math.Long.
 */
var $hs_UTF8_REJECT = 12

var $hs_utf8d =
   [
  /*
   * The first part of the table maps bytes to character classes that
   * to reduce the size of the transition table and create bitmasks.
   */
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
   7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
   8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

  /*
   * The second part is a transition table that maps a combination of
   * a state of the automaton and a character class to a state.
   */
   0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
  12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
  12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
  12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
  12,36,12,12,12,12,12,12,12,12,12,12];

function $hs_decode(state, cp, b) {
  var type = $hs_utf8d[b];

  var codep = (state != $hs_UTF8_ACCEPT) ?
    (b & 0x3f) | (cp << 6) :
    (0xff >>> type) & b;

  return [$hs_utf8d[256 + state + type], codep];
};
/*
 * A best-effort decoder. Runs until it hits either end of input or
 * the start of an invalid byte sequence.
 *
 * At exit, updates *destoff with the next offset to write to, and
 * returns the next source offset to read from.
 */
function _hs_text_decode_utf8(dest, doffptr, src, srcend)
{
    // Assumes src[0] is srcend[0]
    var srcarray = new Uint8Array(src[0],0);
    var s = src[1];
    var send = srcend[1];
    var destarray = new Uint16Array(dest[0],dest[1]);
    // Should really check the size of size_t and CSize here somehow
    var doffArray = new Uint32Array(doffptr[0], doffptr[1]);
    var doff = doffArray[0];
    var state = $hs_UTF8_ACCEPT;

    var codepoint = 0;
    while (s != srcend) {
        var next = $hs_decode(state, codepoint, srcarray[s++]);
        state = next[0];
        codepoint = next[1];
        if ( state != $hs_UTF8_ACCEPT) {
            if (state != $hs_UTF8_REJECT)
                continue;
            break;
        }

        if ((codepoint >>> 0) <= 0xffff)
            destarray[doff++] = codepoint;
        else {
            destarray[doff++] = 0xD7C0 + (codepoint >>> 10);
            destarray[doff++] = 0xDC00 + (codepoint & 0x3FF);
        }
    }

    /* Error recovery - if we're not in a valid finishing state, back up. */
    if (state != $hs_UTF8_ACCEPT)
        s -= 1;

    doffArray[0] = doff;

    return [src[0], s, src[2] + (s - src[1])];
};
function $hs_fromUtf8(src) {
    var res = "";
    var srcarray = new Uint8Array(src[0],0);
    var s = src[1];
    var state = $hs_UTF8_ACCEPT;

    var codepoint = 0;
    while (srcarray[s] != 0) {
        var next = $hs_decode(state, codepoint, srcarray[s++]);
        state = next[0];
        codepoint = next[1];
        if ( state != $hs_UTF8_ACCEPT) {
            if (state != $hs_UTF8_REJECT)
                continue;
            break;
        }

        if ((codepoint >>> 0) <= 0xffff)
            res += String.fromCharCode(codepoint);
        else {
            res += String.fromCharCode(0xD7C0 + (codepoint >>> 10));
            res += String.fromCharCode(0xDC00 + (codepoint & 0x3FF));
        }
    }

    /* Error recovery - if we're not in a valid finishing state, back up. */
//    if (state != $hs_UTF8_ACCEPT)
//        throw maybe ?;

    return res;
};
function $hs_toUtf8(str) {
    return lib.encodeUTF8(str);
};
function memcpy(dest, src, count) {
    if(typeof(src) === 'string') {
        var destarray = new Uint8Array(dest[0],dest[1]);
        var soff = 0;
        var doff = 0;
        while(count != 0) {
            destarray[doff] = src[soff];
            soff++;
            doff++;
            count--;
        }
        return dest;
    }
    else {
        var destarray = new Uint8Array(dest[0],dest[1]);
        var srcarray = new Uint8Array(src[0],src[1]);
        var doff = 0;
        var soff = 0;
        while(count != 0) {
            destarray[doff] = srcarray[soff];
            soff++;
            doff++;
            count--;
        }
        return dest;
    }
};
function memcmp(a, b, count) {
    if(typeof(a) === 'string') {
        if(typeof(b) === 'string') {
            var x = a.slice(0,count).localeCompare(b.slice(0,count));
            return $hs_int(x === 0 ? 0 : (x < 0 ? -1 : 1));
        }
        var barray = new Uint8Array(b[0],b[1]);
        var aoff = 0;
        var boff = 0;
        while(count != 0) {
            if( a[aoff] < barray[boff] )
                return $hs_int(-1);
            if( a[aoff] > barray[boff] )
                return $hs_int(1);
            aoff++;
            boff++;
            count--;
        }
        return $hs_int(0);
    }
    else {
        if(typeof(b) === 'string') {
            var aarray = new Uint8Array(a[0],a[1]);
            var aoff = 0;
            var boff = 0;
            while(count != 0) {
                if( aarray[aoff] < b[boff] )
                    return $hs_int(-1);
                if( aarray[aoff] > b[boff] )
                    return $hs_int(1);
                aoff++;
                boff++;
                count--;
            }
            return $hs_int(0);
        }
        var aarray = new Uint8Array(a[0],a[1]);
        var barray = new Uint8Array(b[0],b[1]);
        var aoff = 0;
        var boff = 0;
        while(count != 0) {
            if( aarray[aoff] < barray[boff] )
                return $hs_int(-1);
            if( aarray[aoff] > barray[boff] )
                return $hs_int(1);
            aoff++;
            boff++;
            count--;
        }
        return $hs_int(0);
    }
};

var $hs_unicodeCat = null;

function u_gencat(a) {
    if($hs_unicodeCat == null) {
        $hs_unicodeCat = [];
        // concatMap (\run ->
        //    case run of
        //        [a]   -> [chr (a+64)]
        //        (a:_) -> show (length run) ++ [chr (a+64)]) . group $ map (fromEnum . generalCategory) ['\x00'..'\xFFFF']
        var s = "32YV3QS3QMNQRQL2Q10H2Q3R2Q26@MQNTKT26AMRNR33YVQ4S2UTUAORZUTUR2JTAUQTJAP3JQ23@R7@24AR8A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@2A@A@A@A@A@A@A@A@2A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A2@A@A@3A2@A@A2@A3@2A4@A2@A3@3A2@A2@A@A@A2@A@2A@A2@A3@A@A2@2AD@3A4D@BA@BA@BA@A@A@A@A@A@A@A@2A@A@A@A@A@A@A@A@A@2A@BA@A3@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@7A2@A2@2A@A4@A@A@A@A@69AD27A18C4T12C14T5C7TCTC17T112E@A@ACT@A2]C3AQ5]2T@Q3@]@]2@A17@]9@35A@2A3@3A@A@A@A@A@A@A@A@A@A@A@A@5A@AR@A2@2A51@48A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@AU5E2G@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A2@A@A@A@A@A@A@2A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A9]38@2]C6Q]39A]QL6]45ELEQ2EQ2EQE8]27D5]3D2Q11]4Z2]3R2QS2Q2U11EQ2]2Q32DC10D21E10H4Q2DE99DQD7EZU6E2C2EU4E2D10H3D2UD14Q]ZDE30D27E2]89D11ED14]10H33D9E2CU3QC5]22D4EC9EC3EC5E2]15Q]25D3E2]Q161]3EF54DEFED3F8E4FE2FD7E10D2E2Q10HQC6D]7D]E2F]8D2]2D2]22D]7D]D3]4D2]ED3F4E2]2F2]2FED8]F4]2D]3D2E2]10H2D2S6JUS5]2EF]6D4]2D2]22D]7D]2D]2D]2D2]E]3F2E4]2E2]3E3]E7]4D]D7]10H2E3DE11]2EF]9D]3D]22D]7D]2D]5D2]ED3F5E]2EF]2FE2]D15]2D2E2]10H]S15]E2F]8D2]2D2]22D]7D]2D]5D2]EDFEF4E2]2F2]2FE8]EF4]2D]3D2E2]10HUD6J10]ED]6D3]3D]4D3]2D]D]2D3]2D3]3D3]12D4]2FE2F3]3F]3FE2]D6]F14]10H3J6USU6]3F]8D]3D]23D]10D]5D3]D3E4F]3E]4E7]2E]2D6]2D2E2]10H8]7JU2]2F]8D]3D]23D]10D]5D2]EDFE5F]E2F]2F2E7]2F7]D]2D2E2]10H]2D15]2F]8D]3D]41D2]D3F4E]3F]3FED8]F8]2D2E2]10H6J3]U6D2]2F]18D3]24D]9D]D2]7D3]E4]3F3E]E]8F18]2FQ12]48DE2D7E4]S6DC8EQ10H2Q37]2D]D2]2D]D2]D6]4D]7D]3D]D]D2]2D]4DE2D6E]2ED2]5D]C]6E2]10H2]2D34]D3U15Q5U2E6U10H10JUEUEUEMNMN2F8D]36D4]14EF5EQ2E5D11E]36E]8UE6U]2U5Q4U2Q37]43D2F4EF6EF2E2F2ED10H6Q6D2F2E4D3ED3F2D7F3D4E13DE2F2E6FEDF10H3FE2U38@10]43DQC3]329D]4D2]7D]D]4D2]41D]4D2]33D]4D2]7D]D]4D2]15D]57D]4D2]67D2]3EU8Q20J3]16D10U6]85D11]L620D2Q17DV26DMN3]75D3Q3I15]13D]4D3E11]18D3E2Q9]18D2E12]13D]3D]2E12]52D2ZF7E8FE2F11E3QC3QSDE2]10H6]10J6]6QL4Q3EV]10H6]35DC52D8]41DED5]70D10]29D3]3E4F2E3F4]2FE6F3E4]U3]2Q10H30D2]5D11]44D4]17F7D2F6]10HJ3]34U23D2E3F2]2Q53DFEF7E]EFE2F8E6F10E2]E10H6]10H6]7QC6Q82]4EF47DEF5EFE5FE2F7D4]10H7Q10U9E9U3]2EF30DF4E2F2EF3]2D10H6]38DEF2E3FEF3E2F8]4Q36D8F8E2F2E3]5Q10H3]3D10H30D6C2Q80]3EQ13EF7E4DE4DF13]44A54C22AC34A37C39E21]4E@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@9A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@9A8@6A2]6@2]8A8@8A8@6A2]6@2]8A]@]@]@]@8A8@14A2]8A8B8A8B8A8B5A]2A4@BTA3T3A]2A4@B3T4A2]2A4@]3T8A5@3T2]3A]2A4@B2T]11V5Z6L2QOPM2OPMO8QWX5ZV9QOP4Q2K3QRMN11QRQK10QV5Z5]6ZJC2]6J3RMNC10J3RMN]13C3]26S22]13E4GE3G12E15]2U@4U@2UA3@2A3@AU@2UR5@6U@U@U@U4@UA4@A4DA2U2A2@5R@4AUR2UAU16J35I@A4IJ6]5R5U2R4UR2UR2UR7UR31U2R2URUR31U268R8U4R20U2R7UMN81UR30U25R40U6R18U12]39U25]11U21]60J78U22J183UR9UR54U8R111UR144U]103UMNMNMNMNMNMNMN30J44U5RMN4R]R]24RMNMNMNMNMN16R256U131RMNMNMNMNMNMNMNMNMNMNMN63RMNMN32RMN258R48U21R2U6R3]10U166]47@]47A]@A3@2A@A@A@A4@A@2A@7AC3@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@2A6U@A@A3E7]4QJ2Q38A10]54D9]CQ14]E23D9]7D]7D]7D]7D]7D]7D]7D]7D]32E2QOPOP3QOPQOP9QL2QLQOP2QOPMNMNMNMN5QC2Q78]26U]89U12]214U26]12U4]V3QUCDIMNMNMNMNMN2UMNMNMNMNLM2NU9I6EL5C2U3ICDQ2U]86D2]2E2T2CDL90DQ3CD5]41D3]94D]2U4J10U27D5]36U12]16D31U]10J39U15J32U10J39U15J63U]256U6582D10]64U20940D52]21DC1143D3]55U9]40D6C2Q268DC3Q16D10H2D20]@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@ADE3GQ8]2EQC@A@A@A@A@A@A@A@A@A@A@A@A8]70D10I2E6Q8]23T9C2T@A@A@A@A@A@A@3A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@A@AC8A@A@A2@A@A@A@A@AC2T@A@A]@A14]@A@A@A@A@A80]A7DE3DE4DE23D2F2EF4U4]6J2USU6]52D4Q8]2F50D16FE9]2Q10H6]18E6D3QD4]10H28D8E2Q23D11E2F11]Q29D3]3EF47DE2F4E2FE4F13Q]C10H4]2Q32]41D6E2F2E2F2E9]3DE8DEF2]10H2]4Q16DC6D3UDF4]48DED3E2D2E5D2EDED24]2DC2Q33]6D2]6D2]6D9]7D]7D145]35D2FE2FE2FQFE2]10H6]11172D12]23D4]49D4]2048[6400\\302D2]62D2]106D38]7A12]5A5]DE10DR13D]5D]D]2D]2D]108D16T17]363DMN16]64D2]54D40]12DSU2]16E7QMNQ6]7E9]Q2L2KMNMNMNMNMNMNMNMN2QMN4Q3K3Q]4QLMNMNMN3QRL3R]QS2Q4]5D]135D2]Z]3QS3QMNQRQL2Q10H2Q3R2Q26@MQNTKT26AMRNRMNQMN2Q10DC45D2C31D3]6D2]6D2]6D2]3D3]2SRTU2S]U4R2U10]3Z2U2]";
        for(var n = 0; n != s.length; n++) {
            var l = '';
            while(goog.string.isNumeric(s[n]))
                l = l + s[n++];
            l = (l === '') ? 1 : (l | 0);
            var c = s[n].charCodeAt() - 64;
            for(var x = 0; x !== l; x++)
                $hs_unicodeCat[$hs_unicodeCat.length] = c;
        }
    }
    return $hs_int($hs_unicodeCat[a]);
};
function u_iswalpha(a) {
    return goog.string.isAlpha(String.fromCharCode(a)) ?
        (WORD_SIZE_IN_BITS == 32 ? 1 : goog.math.Long.ONE)
       :(WORD_SIZE_IN_BITS == 32 ? 0 : goog.math.Long.ZERO);
};
function u_iswalnum(a) {
    return goog.string.isAlphaNumeric(String.fromCharCode(a)) ?
        (WORD_SIZE_IN_BITS == 32 ? 1 : goog.math.Long.ONE)
       :(WORD_SIZE_IN_BITS == 32 ? 0 : goog.math.Long.ZERO);
};
function u_iswspace(a) {
    return '\t\n\v\f\r \u0020\u00a0\u1680\u180e\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u202f\u205f\u3000'
        .indexOf(String.fromCharCode(a)) != -1 ?
        (WORD_SIZE_IN_BITS == 32 ? 1 : goog.math.Long.ONE)
       :(WORD_SIZE_IN_BITS == 32 ? 0 : goog.math.Long.ZERO);
};
function u_iswlower(a) {
    return a !== u_towupper(a) ?
        (WORD_SIZE_IN_BITS == 32 ? 1 : goog.math.Long.ONE)
       :(WORD_SIZE_IN_BITS == 32 ? 0 : goog.math.Long.ZERO);
};
function u_iswupper(a) {
    return a !== u_towlower(a) ?
        (WORD_SIZE_IN_BITS == 32 ? 1 : goog.math.Long.ONE)
       :(WORD_SIZE_IN_BITS == 32 ? 0 : goog.math.Long.ZERO);
};
function u_towlower(a) { return $hs_int(String.fromCharCode(a).toLowerCase().charCodeAt()); };
function u_towupper(a) { return $hs_int(String.fromCharCode(a).toUpperCase().charCodeAt()); };
function rtsSupportsBoundThreads() { return $hs_int(0); };
function getOrSetGHCConcSignalSignalHandlerStore(x) { return x; };
function stg_sig_install(a,b,c) { return $hs_int(-1); };
function localeEncoding() { return $hs_ascii("UTF-32LE"); };
function hs_iconv_open(to,from) { return $hs_int(1); };
function hs_iconv_close(h) { return $hs_int(0); };

function $hs_unsignedCompare(a,b) {
    if (a.equals(b)) return $hs_int(0);

    var aneg = a.isNegative();
    var bneg = b.isNegative();
    if (aneg && !bneg) return $hs_int(1);
    if (!aneg && bneg) return $hs_int(-1);

    return $hs_int(a.subtract(b).isNegative() ? -1 : 1);
};


if(WORD_SIZE_IN_BITS == 32) {
    // Safer 32 bit multiplication than just a * b
    function $hs_timesIntzh(a, b) {
       return goog.math.Long(a,0).multiply(goog.math.Long(b,0)).getLowBits(); };
}

if(WORD_SIZE_IN_BITS == 64) {
    // Int primatives for 64bit
    function $hs_quotIntzh(a, b) {
       return a.div(b); };
    function $hs_remIntzh(a, b) {
       return a.modulo(b); };
    function $hs_int2Wordzh(a) {
       return a; };
    function $hs_int2Floatzh(a) {
       return a.toNumber(); };
    function $hs_int2Doublezh(a) {
       return a.toNumber(); };
    function $hs_uncheckedIShiftLzh(a, b) {
       return a.shiftLeft(b.toNumber()); };
    function $hs_uncheckedIShiftRAzh(a, b) {
       return a.shiftRight(b.toNumber()); };
    function $hs_uncheckedIShiftRLzh(a, b) {
       return a.shiftRight(b.toNumber()); };

    // Word primatives for 64bit
    function $hs_quotWordzh(a, b) {
       return a.div(b); };   // TODO make unsigned
    function $hs_remWordzh(a, b) {
       return a.modulo(b); }; // TODO make unsigned
    function $hs_uncheckedShiftLzh(a, b) {
       return a.shiftLeft(b.toNumber()); };
    function $hs_uncheckedShiftRLzh(a, b) {
       return a.shiftRight(b.toNumber()); };
    function $hs_word2Intzh(a) {
       return a; };
    function $hs_gtWordzh(a, b) {
       return $hs_mkBool($hs_unsignedCompare(a, b) > 0); };
    function $hs_geWordzh(a, b) {
       return $hs_mkBool($hs_unsignedCompare(a, b) >= 0); };
    function $hs_eqWordzh(a, b) {
       return $hs_mkBool(a.equals(b)); };
    function $hs_neWordzh(a, b) {
       return $hs_mkBool(a.notEquals(b)); };
    function $hs_ltWordzh(a, b) {
       return $hs_mkBool($hs_unsignedCompare(a, b) < 0); };
    function $hs_leWordzh(a, b) {
       return $hs_mkBool($hs_unsignedCompare(a, b) <= 0); };

    function $hs_intToIntzh(i) {
       return goog.math.Long.fromInt(i); };
    function $hs_intzhToInt(i) {
       return i.toInt(); };
    function $hs_int64ToWordzh(i) {
       return i; };
    function $hs_wordToWordzh(i) {
       return goog.math.Long.fromBits(i,0); };
    function $hs_wordzhToWord(i) {
       return i.getLowBits(); };
    function $hs_word64ToIntzh(i) {
       return i; };

    function $hs_narrow8Intzh(i) {
       low = i.getLowBits();
       return goog.math.Long.fromNumber((low & 0x7F) - (low & 0x80)); };
    function $hs_narrow16Intzh(i) {
       low = i.getLowBits();
       return goog.math.Long.fromNumber((low & 0x7FFF) - (low & 0x8000)); };
    function $hs_narrow32Intzh(i) {
       return goog.math.Long.fromNumber(i.getLowBits()|0); };
    function $hs_narrow8Wordzh(i) {
       return goog.math.Long.fromBits(i.getLowBits() & 0xFF, 0); };
    function $hs_narrow16Wordzh(i) {
       return goog.math.Long.fromBits(i.getLowBits() & 0xFFFF, 0); };
    function $hs_narrow32Wordzh(i) {
       return goog.math.Long.fromBits(i.getLowBits(), 0); };
}

function hs_gtWord64(a, b) {
   return $hs_unsignedCompare(a, b) > 0 ? 1 : 0; };
function hs_geWord64(a, b) {
   return $hs_unsignedCompare(a, b) >= 0 ? 1 : 0; };
function hs_eqWord64(a, b) {
   return a.equals(b) ? 1 : 0; };
function hs_neWord64(a, b) {
   return a.notEquals(b) ? 1 : 0; };
function hs_ltWord64(a, b) {
   return $hs_unsignedCompare(a, b) < 0 ? 1 : 0; };
function hs_leWord64(a, b) {
   return $hs_unsignedCompare(a, b) <= 0 ? 1 : 0; };

function hs_gtInt64(a, b) {
   return a.greaterThan(b) ? 1 : 0; };
function hs_geInt64(a, b) {
   return a.greaterThanOrEqual(b) ? 1 : 0; };
function hs_eqInt64(a, b) {
   return a.equals(b) ? 1 : 0; };
function hs_neInt64(a, b) {
   return a.notEquals(b) ? 1 : 0; };
function hs_ltInt64(a, b) {
   return a.lessThan(b) ? 1 : 0; };
function hs_leInt64(a, b) {
   return a.lessThanOrEqual(b) ? 1 : 0; };

function hs_remWord64(a, b) {
   return a.modulo(b); }; // TODO make unsigned
function hs_quotWord64(a, b) {
   return a.div(b); };   // TODO make unsigned

function hs_remInt64(a, b) {
   return a.modulo(b); };
function hs_quotInt64(a, b) {
   return a.div(b); };
function hs_negateInt64(a) {
   return a.negate(); };
function hs_plusInt64(a, b) {
   return a.add(b); };
function hs_minusInt64(a, b) {
   return a.subtract(b); };
function hs_timesInt64(a, b) {
   return a.multiply(b); };

function hs_and64(a, b) {
   return a.and(b); };
function hs_or64(a, b) {
   return a.or(b); };
function hs_xor64(a, b) {
   return a.xor(b); };
function hs_not64(a) {
   return a.not(); };

function hs_uncheckedShiftL64(a, b) {
   return a.shiftLeft(b); };
function hs_uncheckedShiftRL64(a, b) {
   return a.shiftRight(b); };
function hs_uncheckedIShiftL64(a, b) {
   return a.shiftLeft(b); };
function hs_uncheckedIShiftRA64(a, b) {
   return a.shiftRight(b); };
function hs_uncheckedIShiftRL64(a, b) {
   return a.shiftRight(b); };

function hs_intToInt64(i) {
   return goog.math.Long.fromInt(i); };
function hs_int64ToInt(i) {
   return i.toInt(); };
function hs_int64ToWord64(i) {
   return i; };
function hs_wordToWord64(i) {
   return goog.math.Long.fromBits(i,0); };
function hs_word64ToWord(i) {
   return i.getLowBits(); };
function hs_word64ToInt64(i) {
   return i; };

errno = 0;
function __hscore_get_errno() {
    HS_RTS_TRACE && $hs_logger.info('__hscore_get_errno');
    return $hs_int(errno);
}
function __hscore_set_errno(e) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_set_errno');
    errno = e;
};
function strerror(e) {
    return $hs_utf32("Error "+e);
}
function __hscore_s_isreg(m) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_s_isreg');
    return $hs_int(1);
};
function __hscore_s_isdir(m) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_s_isdir');
    return $hs_int(0);
};
function __hscore_s_isfifo(m) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_s_isfifo');
    return $hs_int(0);
};
function __hscore_s_isblk(m) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_s_isblk');
    return $hs_int(0);
};
function __hscore_s_ischr(m) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_s_ischr');
    return $hs_int(0);
};
function __hscore_s_issock(m) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_s_issock');
    return $hs_int(0);
};

function __hscore_sigemptyset(set) { return $hs_int(0); };
function __hscore_sigfillset(set) { return $hs_int(0); };
function __hscore_sigaddset(set, s) { return $hs_int(0); };
function __hscore_sigdelset(set, s) { return $hs_int(0); };
function __hscore_sigismember(set, s) { return $hs_int(0); };

function __hscore_memcpy_src_off(dest, src, soff, count) {
    var doff = 0;
    var srcarray = new Uint8Array(src[0]);
    var destarray = new Uint8Array(dest[0]);
    while(count != 0) {
        destarray[doff] = srcarray[soff];
        soff++;
        doff++;
        count--;
    }
};
function __hscore_bufsiz() { return $hs_int(1024); };
function __hscore_seek_cur() { return $hs_int(1); };
function __hscore_seek_set() { return $hs_int(0); };
function __hscore_seek_end() { return $hs_int(2); };

function __hscore_o_binary() { return $hs_int(0); };
function __hscore_o_rdonly() { return $hs_int(0); };
function __hscore_o_wronly() { return $hs_int(0x0001); };
function __hscore_o_rdwr() { return $hs_int(0x0002); };
function __hscore_o_append() { return $hs_int(0x0008); };
function __hscore_o_creat() { return $hs_int(0x0200); };
function __hscore_o_excl() { return $hs_int(0x0800); };
function __hscore_o_trunc() { return $hs_int(0x0400); };
function __hscore_o_noctty() { return $hs_int(0x20000); };
function __hscore_o_nonblock() { return $hs_int(0x0004); };

function __hscore_ftruncate(fd, where) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_ftruncate');
    return $hs_int(0);
};
function __hscore_setmode(fd, toBin) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_setmode');
    return $hs_int(0);
};

function __hscore_sizeof_stat() { return $hs_int(4); };
function __hscore_st_mtime(st) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_st_mtime');
    return $hs_int(0);
};
function __hscore_st_size(st) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_st_size');
    return $hs_int(0);
};
function __hscore_st_mode(st) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_st_mode');
    return $hs_int(0);
};
function __hscore_st_dev(st) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_st_dev');
    return $hs_int(0);
};
function __hscore_st_ino(st) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_st_ino');
    return $hs_int(0);
};
function __hscore_stat(f, buf) {
    var p = $hs_fromUtf32(f);
    HS_RTS_TRACE && $hs_logger.info('__hscore_stat '+p);
    return $hs_int(0);
};
function __hscore_fstat(fd, buf) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_fstat');
    return $hs_int(0);
};
function __hscore_lstat(f, buf) {
    var p = $hs_fromUtf32(f);
    HS_RTS_TRACE && $hs_logger.info('__hscore_lstat '+p);
    return $hs_int(0);
};

function __hscore_lflag(ts) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_fstat');
    return ts.c_lflag;
};
function __hscore_poke_lflag(ts, t) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_poke_lflag');
    ts.c_lflag = t;
};
function __hscore_ptr_c_cc(ts) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_ptr_c_cc');
    return ts.c_cc;
};
function __hscore_sizeof_termios() { return $hs_int(0); };
function __hscore_sizeof_sigset_t() { return $hs_int(0); };

function __hscore_echo() { return $hs_int(0); };
function __hscore_tcsanow() { return $hs_int(0); };
function __hscore_icanon() { return $hs_int(0); };
function __hscore_vmin() { return $hs_int(0); };
function __hscore_vtime() { return $hs_int(0); };
function __hscore_sigttou() { return $hs_int(0); };
function __hscore_sig_block() { return $hs_int(0); };
function __hscore_sig_setmask() { return $hs_int(0); };
function __hscore_sizeof_siginfo_t() { return $hs_int(0); };
function __hscore_f_getfl() { return $hs_int(0); };
function __hscore_f_setfl() { return $hs_int(0); };
function __hscore_f_setfd() { return $hs_int(0); };
function __hscore_fd_cloexec() { return $hs_int(0); };
function __hscore_get_saved_termios(fd) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_get_saved_termios');
    return null;
};
function __hscore_set_saved_termios(fd, ts) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_set_saved_termios');
};
function __hscore_hs_fileno(f) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_hs_fileno');
    return $hs_int(0);
};
var $hs_fstab = [];
function $hs_getFileURL(f) {
    for(var i=0;i!==$hs_fstab.length;i++) {
        if(f.slice(0, $hs_fstab[i].mountPoint.length) === $hs_fstab[i].mountPoint)
            return $hs_fstab[i].url + f.slice($hs_fstab[i].mountPoint.length);
    }
    return null;
};
function $hs_MemFile(text) {
  this.isatty = false;
  this.text = text;
  this.fptr = 0;
  this.waitingToRead = [];
};
$hs_MemFile.prototype.read = function(p, s) {
    var n = this.text.length - this.fptr;
    if( n <= 0 ) {
        HS_RTS_TRACE && $hs_logger.info('read : end of file');
        return 0;
    }

    var maxChars = s>>>2;
    if( n > maxChars )
        n = maxChars;

    var end = this.fptr + n;
    var dest = new Uint32Array(p[0], p[1]);
    for(var i=this.fptr;i!=end;i++)
        dest[i]=this.text.charCodeAt(i);
    this.fptr=end;

    HS_RTS_TRACE && $hs_logger.info('read : '+(n<<2));
    return n<<2;
};
$hs_MemFile.prototype.write = function(p, s) {
    var len = s>>>2;
    var src = new Uint32Array(p[0], p[1]);
    var res = "";
    for(var i=0;i!=len;i++)
        res=res+String.fromCharCode(src[i]);

    if(this.fptr <= this.text.length)
        this.text=this.text+res;
    else if(this.text.length > this.fptr + len)
        this.text=this.text.slice(0,this.fptr)+res+this.text.slice(this.fptr+len);
    else
        this.text=this.text.slice(0,this.fptr)+res+this.text.slice(this.fptr+len);

    return s;
};
$hs_MemFile.prototype.ready = function() {
  // Do not change to write !== 0 (implicit cast makes it work with goog.math.Long
  return write != 0 || f.text.length > f.fptr;
}
function $hs_ConsoleOut(terminal) {
  this.isatty = true;
};
$hs_ConsoleOut.prototype.read = function(p, s) {
    return 0;
};
$hs_ConsoleOut.prototype.write = function(p, s) {
  var len = s>>>2;
  var src = new Uint32Array(p[0], p[1]);
  var res = "";
  for(var i=0;i!=len;i++) {
    var c = src[i];
    res=res+(c==10?"\r\n":String.fromCharCode(c));
  }
  console.log(res);
  return s;
};
$hs_ConsoleOut.prototype.ready = function() {
  // Do not change to write !== 0 (implicit cast makes it work with goog.math.Long
  return write != 0;
}
function $hs_TerminalOut(terminal) {
  this.isatty = true;
  this.terminal = terminal;
};
$hs_TerminalOut.prototype.read = function(p, s) {
  return 0;
};
$hs_TerminalOut.prototype.write = function(p, s) {
  var len = s>>>2;
  var src = new Uint32Array(p[0], p[1]);
  var res = "";
  for(var i=0;i!=len;i++) {
    var c = src[i];
    res=res+(c==10?"\r\n":String.fromCharCode(c));
  }
  this.terminal.io.print(res);
  return s;
};
$hs_TerminalOut.prototype.ready = function() {
  // Do not change to write !== 0 (implicit cast makes it work with goog.math.Long
  return write != 0;
}
function $hs_TerminalIn(terminal) {
  this.isatty = true;
  this.terminal = terminal;
  this.buffer = "";
  this.waitingToRead = [];
  var _this = this;
  terminal.io.sendString = function(s) {
    if(s=="\r") {
       s="\n";
       terminal.io.print("\r\n");
    }
    else {
       terminal.io.print(s);
    }
    _this.buffer = _this.buffer + s;

    // Wake any waiting threads
    var wake = _this.waitingToRead;
    _this.waitingToRead = [];

    for(var i = 0; i != wake.length; i++)
      wake[i]();
  };
  terminal.io.onVTKeystroke = terminal.io.sendString;
};
$hs_TerminalIn.prototype.read = function(p, s) {
    var n = this.buffer.length;
    if( n <= 0 ) {
        HS_RTS_TRACE && $hs_logger.info('read : end of file');
        return 0;
    }

    var maxChars = s>>>2;
    if( n > maxChars )
        n = maxChars;

    var dest = new Uint32Array(p[0], p[1]);
    for(var i=0;i!=n;i++)
        dest[i]=this.buffer.charCodeAt(i);

    this.buffer = this.buffer.substr(i);

    HS_RTS_TRACE && $hs_logger.info('read : '+(n<<2));
    return n<<2;
};
$hs_TerminalIn.prototype.write = function(p, s) {
  return -1;
};
$hs_TerminalIn.prototype.ready = function(write) {
  // Do not change to write === 0 (implicit cast makes it work with goog.math.Long
  return write == 0 && this.buffer.length != 0;
}
var $hs_allFiles = [
    new $hs_MemFile(""), // stdin
    new $hs_MemFile(""), // stdout
    new $hs_MemFile("")  // stderr
];
function $hs_setTerminal(terminal) {
  $hs_allFiles[0] = new $hs_TerminalIn(terminal);
  $hs_allFiles[1] = new $hs_TerminalOut(terminal);
  $hs_allFiles[2] = new $hs_TerminalOut(terminal);
};
function fdReady(fd, write) {
    HS_RTS_TRACE && $hs_logger.info('fdReady');
    var f = $hs_allFiles[fd];
    if (f === undefined || f === null)
        return $hs_int(-1);
    return $hs_int(f.ready(write) ? 1 : 0);
};
function $hs_findFile(f) {
    for(var i=0;i!==$hs_allFiles.length;i++) {
        if(f===$hs_allFiles[i].path)
            return i;
    }
    return -1;
};
function __hscore_open(f,h,m) {
    var p = $hs_fromUtf32(f);
    HS_RTS_TRACE && $hs_logger.info('__hscore_open '+p);
    var result=$hs_findFile(p);
    if(result===-1) {
        var url = $hs_getFileURL(p);
        if(url!==null) {
            try {
                var transport = new XMLHttpRequest();
                transport.open("GET", url, false);
                transport.send(null);
                if (transport.status == 200 || transport.status == 0) {
                    result = $hs_allFiles.length;
                    var newFile = new $hs_MemFile(transport.responseText);
                    newFile.path = p;
                    $hs_allFiles[result] = newFile;
                }
                else {
                   $hs_logError("Error " + transport.status + " opening file: " + p +" ( " + url + " )");
                }
            } catch (e) {
                $hs_logError("Error opening file: " + p + " ( " + url + " ) :\n" + e);
            }
        }
        else {
            if(m & __hscore_o_creat() !== 0) {
                result = $hs_allFiles.length;
                var newFile = new $hs_MemFile("");
                newFile.path = p;
                $hs_allFiles[result] = newFile;
            }
        }
    }
    return $hs_int(result);
};
function close(fd) {
    HS_RTS_TRACE && $hs_logger.info('close');
    return $hs_int(0);
};
function __hscore_lseek(fd, off, whence) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_lseek');
    return $hs_int(0);
};

function hsFD_SETSIZE() { return $hs_int(1024); };
function hsFD_ISSET(fd, fds) {
    HS_RTS_TRACE && $hs_logger.info('hsFD_ISSET');
    return $hs_int(0);
};
function hsFD_SET(fd, fds) {
    HS_RTS_TRACE && $hs_logger.info('hsFD_SET');
};
function sizeof_fd_set() {
    HS_RTS_TRACE && $hs_logger.info('sizeof_fd_set');
    return $hs_int(0);
};
function hsFD_ZERO(fds) {
    HS_RTS_TRACE && $hs_logger.info('hsFD_ZERO');
};

function __hscore_select(nfds, readfds, writefds, exceptfds, timeout) {
    HS_RTS_TRACE && $hs_logger.info('hsFD_ZERO');
    return $hs_int(0);
};

environ = {};
environ['TMPDIR'] = $hs_utf32("/tmp");
function __hscore_environ() {
    HS_RTS_TRACE && $hs_logger.info('__hscore_environ');
    return environ;
};
function getenv(e) {
    var s = $hs_fromUtf32(e);
    HS_RTS_TRACE && $hs_logger.info('getenv '+s);
    var v = environ[s];
    return v===undefined?null:v;
};
function lockFile(fd, dev, ino, for_writing) {
    HS_RTS_TRACE && $hs_logger.info('lockFile');
    return $hs_int(0);
};
function unlockFile(fd) {
    HS_RTS_TRACE && $hs_logger.info('unlockFile');
    return $hs_int(0);
};
function isatty(fd) {
    HS_RTS_TRACE && $hs_logger.info('isatty');
    var f = $hs_allFiles[fd];
    if (f === undefined || f === null)
        return $hs_int(-1);
    return $hs_int(f.isatty ? 1 : 0);
};
function read(fd, p, s) {
    HS_RTS_TRACE && $hs_logger.info('read');
    var f = $hs_allFiles[fd];
    if (f === undefined || f === null)
        return $hs_int(-1);
    return $hs_int(f.read(p, s));
};
function write(fd, p, s) {
    HS_RTS_TRACE && $hs_logger.info('write');
    var f = $hs_allFiles[fd];
    if (f === undefined || f === null)
        return $hs_int(-1);
    return $hs_int(f.write(p, s));
};
function ghc_strlen(s) {
    return s.indexOf('\x00');
};
function initLinker() {
    HS_RTS_TRACE && $hs_logger.info('initLinker');
};
function exitLinker() {
    HS_RTS_TRACE && $hs_logger.info('exitLinker');
};
var $hs_loaded = [];
var $hs_loadPath = "./";
var $hs_loading = false;

/**
 * @param {Array.<Object>}      args
 * @param {function(!string)}   onComplete
 * @param {function(!Object)=}  onException
 */
function $hs_fromString(args, onComplete, onException) {
    var loop = function(res, a) {
        $hs_force(a, function(s) {
            switch (s.g) {
                case 1: // nil
                    onComplete(res);
                    break;
                case 2: // cons
                    var chthunk = s.v[0];
                    var sthunk = s.v[1];
                    $hs_force([chthunk], function(ch){
                        loop(res+ch.v[0],[sthunk]);}, onException);
                    break;
                default:
                    throw "undefined";
            }
        }, onException);
    }
    loop("", args);
};
/**
 * @param {string} s
 * @return {!Object}
 */
function $hs_toString(s) {
    var x = $d(1, []);
    for(var i=s.length;i!=0;)
        x=$d(2, [$d(1, [s.charAt(--i)]), x]);
    return x;
};
/**
 * @param {Array.<Object>}      args
 * @param {function(!string)}   onComplete
 * @param {function(!Object)=}  onException
 */
function $hs_fromText(args, onComplete, onException) {
    $hs_force(args, function(s) {
        if(HS_DEBUG && s.g !== 1) throw "Invalid Text";
        var arr = s.v[0];
        var off = WORD_SIZE_IN_BITS == 32 ? s.v[1] : s.v[1].toNumber();
        var end = off + WORD_SIZE_IN_BITS == 32 ? s.v[2] : s.v[2].toNumber();
        var buff = new Uint16Array(arr[0]);
        var result = "";
        for(var n = off; n !== end; n++)
            result += String.fromCharCode(buff[n]);
        onComplete(result);
    }, onException);
};
/**
 * @param {Array.<Object>}      args
 * @param {function(!string)}   onComplete
 * @param {function(!Object)=}  onException
 */
function $hs_fromLazyText(args, onComplete, onException) {
    var loop = function(res, a) {
        $hs_force(a, function(s) {
            switch (s.g) {
                case 1: // Empty
                    onComplete(res);
                    break;
                case 2: // Chunk
                    var arr = s.v[0];
                    var off = WORD_SIZE_IN_BITS == 32 ? s.v[1] : s.v[1].toNumber();
                    var end = off + WORD_SIZE_IN_BITS == 32 ? s.v[2] : s.v[2].toNumber();
                    var buff = new Uint16Array(arr[0]);
                    for(var n = off; n !== end; n++)
                        res += String.fromCharCode(buff[n]);
                    loop(res,[s.v[3]]);
                    break;
                default:
                    throw "undefined";
            }
        }, onException);
    }
    loop("", args);
};
/**
 * @param {string} s
 * @return {!Object}
 */
function $hs_toText(s) {
    var a = $hs_newByteArrayzh($hs_int((s.length << 1) + 2))[1];
    var dest = new Uint16Array(a[0]);
    for(var i=0;i!=s.length;i++)
        dest[i]=s.charCodeAt(i);
    dest[i]=0;
    return $d(1, [a,
        WORD_SIZE_IN_BITS == 32 ? 0 : goog.math.Long.ZERO,
        WORD_SIZE_IN_BITS == 32 ? s.length : goog.math.Long.fromNumber(s.length)]);
};
/**
 * @param {string} s
 * @return {!Object}
 */
function $hs_toLazyText(s) {
    var a = $hs_newByteArrayzh($hs_int((s.length << 1) + 2))[1];
    var dest = new Uint16Array(a[0]);
    for(var i=0;i!=s.length;i++)
        dest[i]=s.charCodeAt(i);
    dest[i]=0;
    return $d(2, [a,
        WORD_SIZE_IN_BITS == 32 ? 0 : goog.math.Long.ZERO,
        WORD_SIZE_IN_BITS == 32 ? s.length : goog.math.Long.fromNumber(s.length),
        $d(1, [])]);
};
/**
 * @param {Array.<Object>}      args
 * @param {function(!number)}   onComplete
 * @param {function(!Object)=}  onException
 */
function $hs_fromInt(args, onComplete, onException) {
    $hs_force(args, function(i){
        if(HS_DEBUG && i.g !== 1) throw "Invalid Int"
        onComplete(WORD_SIZE_IN_BITS == 32 ? i.v[0] : i.v[0].toNumber());}, onException);
};
/**
 * @param {Array.<Object>}      args
 * @param {function(!Object)}   onComplete
 * @param {function(!Object)=}  onException
 */
function $hs_runIO(args, onComplete, onException) {
    var newArguments = [];
    for (var i = 0; i < args.length; i++)
        newArguments[i] = args[i];
    newArguments[args.length] = $$GHCziPrim_realWorldzh;
    $hs_force(newArguments,
      onComplete === undefined ? undefined : function(i){onComplete(i[1]);},
      onException);
};
/**
 * @param {number} i
 */
function $hs_toInt(i) {
    return $d(1, [WORD_SIZE_IN_BITS == 32 ? i|0 : goog.math.Long.fromNumber(i)]);
};
function hs_nil() {
    return "";
};
function hs_cons(x, xs) {
    return String.fromCharCode(x) + xs;
};
var $$GHCziPrim_realWorldzh = 0;
var $$GHCziPrim_coercionTokenzh = 0;
function $hs_init() {
    $$GHCziTypes_False = $d(1, []);
    $$GHCziTypes_True = $d(2, []);
};
/**
 * @param {Array.<Object>}      args
 * @param {function(!Object)}   onComplete
 * @param {function(!Object)=}  onException
 */
function $hs_consoleInitAndRunIO(args, onComplete, onException) {
  $hs_allFiles[1] = new $hs_ConsoleOut();
  $hs_allFiles[2] = new $hs_ConsoleOut();
  $hs_runIO(args, onComplete, onException);
};
/**
 * @param {Array.<Object>}      args
 * @param {function(!Object)}   onComplete
 * @param {function(!Object)=}  onException
 */
function $hs_htermInitAndRunIO(args, onComplete, onException) {
  function execHaskell() {
    var profileName = lib.f.parseQuery(document.location.search)['profile'];
    var terminal = new hterm.Terminal(profileName);
    terminal.decorate(document.querySelector('#terminal'));

    $hs_setTerminal(terminal);

    // Useful for console debugging.
    window.term_ = terminal;

    setTimeout(function() {
        terminal.setCursorPosition(0, 0);
        terminal.setCursorVisible(true);
        terminal.installKeyboard();
        $hs_runIO(args, onComplete, onException);
      }, 0);
  }

  // lib.ensureRuntimeDependencies();
  hterm.init(execHaskell);
};
function $hs_nodeInitAndRunIO(args, onComplete, onException) {
  $hs_init();
  $hs_consoleInitAndRunIO([$$$Main_main],
    onComplete !== undefined ? onComplete : function(res) {
      $hs_force([res], function(i) {
          if(i.g === 1)
          process.exit(WORD_SIZE_IN_BITS == 32 ? i.v[0] : i.v[0].toNumber());
      });
    },
    onException !== undefined ? onException : function(e) {
      console.log("Error : " + e);
    });
};
function MD5Init(ctx) {
    ctx.googCtx = new goog.crypt.Md5();
};
function MD5Update(ctx, dat, len) {
    var i8 = new Int8Array(dat[0]);
    ctx.googCtx.update(i8, len);
};
function MD5Final(dst, ctx) {
    var digest = ctx.googCtx.digest();
    var i8 = new Int8Array(dst);
    for(var i=0;i<16;i++) {
      i8[i] = digest[i];
    }
};
function gettimeofday(tp, tzp) {
    var x = new Int32Array(tp[0], tp[1]);
    var t = new Date().getTime();
    if(WORD_SIZE_IN_BITS==32) {
        x[0] = (t/1000) | 0;
        x[1] = ((t%1000)*1000) | 0;
        return 0;
    }
    else {
        x[0] = (t/1000) | 0;
        x[1] = 0;
        x[2] = ((t%1000)*1000) | 0;
        x[3] = 0;
        return goog.math.Long.ZERO;
    }
};
function ghc_wrapper_d1rx_getrusage(who, usage) {
    var x = new Int32Array(usage[0], usage[1]);
    var t = new Date().getTime();
    if(WORD_SIZE_IN_BITS==32) {
        x[0] = (t/1000) | 0;
        x[1] = ((t%1000)*1000) | 0;
        return 0;
    }
    else {
        x[0] = (t/1000) | 0;
        x[1] = 0;
        x[2] = ((t%1000)*1000) | 0;
        x[3] = 0;
        return goog.math.Long.ZERO;
    }
};
function g_object_ref(p) {
  return p;
};
function g_free(p) {
}
function ghcjs_currentWindow() {
  return window;
};
function ghcjs_currentDocument() {
  return document;
};
function webkit_web_view_get_dom_document(w) {
  return w.document;
};
function gtk2hs_closure_new(f) {
  return f;
};
function g_type_check_instance_is_a(o, t) {
  return $hs_int(o instanceof t ? 1 : 0);
};
function webkit_dom_event_target_add_event_listener_closure(obj, eventName, f, bubble) {
  obj.addEventListener($hs_fromUtf8(eventName), function(e) {
    $hs_runIO([f, {g:1, v:[obj]}, {g:1,v:[e]}]);
  });
  return $hs_int(1);
};
