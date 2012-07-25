var $hs_mkBool = function(b) {
    return b ? $$GHCziTypes_True:$$GHCziTypes_False;
};
var $hs_tagToEnumzh = function(t) {
    return $d(t+1, []);
};
var $hs_mulIntMayOflozh = function(a, b) {
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
var $hs_ztzh = function(a, b) {
    // Safe 32bit multiply 64 bit multiply is done inline
    return goog.math.Long.fromInt(a).multiply(goog.math.Long.fromInt(b)).getLowBits();
};
var $hs_addIntCzh = function(a, b) {
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
var $hs_subIntCzh = function(a, b) {
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
var $hs_ordzh = function(a) {
    if(WORD_SIZE_IN_BITS==32) {
        return a.charCodeAt();
    }
    else {
        return goog.math.Long.fromNumber(a.charCodeAt());
    }
};
var $hs_chrzh = function(a) {
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
var $hs_popCnt8 = function(a) {
    return $hs_popCntTab[a & 0xFF];
};
var $hs_popCnt8zh = function(a) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_popCnt8(a);
    }
    else {
        return goog.math.Long.fromNumber(
            $hs_popCnt8(a.getLowBits()));
    }
};
var $hs_popCnt16zh = function(a) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_popCnt8(a) + $hs_popCnt8(a>>>8);
    }
    else {
        return goog.math.Long.fromNumber(
            $hs_popCnt8(a.getLowBits())+$hs_popCnt8(a.getLowBits()>>>8));
    }
};
var $hs_popCnt32zh = function(a) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_popCnt8(a) + $hs_popCnt8(a>>>8) + $hs_popCnt8(a>>>16) + $hs_popCnt8(a>>>24);
    }
    else {
        var l = a.getLowBits();
        return goog.math.Long.fromNumber(
            $hs_popCnt8(l)+$hs_popCnt8(l>>>8) + $hs_popCnt8(l>>>16) + $hs_popCnt8(l>>>24));
    }
};
var $hs_popCnt64zh = function(a) {
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
var $hs_expDoublezh = function(a) {
    return Math.exp(a);
};
var $hs_logDoublezh = function(a) {
    return Math.log(a);
};
var $hs_sqrtDoublezh = function(a) {
    return Math.sqrt(a);
};
var $hs_sinDoublezh = function(a) {
    return Math.sin(a);
};
var $hs_cosDoublezh = function(a) {
    return Math.cos(a);
};
var $hs_tanDoublezh = function(a) {
    return Math.tan(a);
};
var $hs_asinDoublezh = function(a) {
    return Math.asin(a);
};
var $hs_acosDoublezh = function(a) {
    return Math.acos(a);
};
var $hs_atanDoublezh = function(a) {
    return Math.atan(a);
};
var $hs_sinhDoublezh = function(a) {
    return (Math.exp(a) - Math.exp(-a)) / 2;
};
var $hs_coshDoublezh = function(a) {
    return (Math.exp(a) + Math.exp(-a)) / 2;
};
var $hs_tanhDoublezh = function(a) {
    var expA = Math.exp(a);
    var expNegA = Math.exp(-a);
    return (expA - expNegA) / (expA + expNegA);
};
var $hs_ztztzhzh = function(a, b) {
    return Math.pow(a,b);
};
var $hs_decodeDoublezu2Intzh = function(x) {
    if( x < 0 ) {
        var result = $hs_decodeDoublezu2Intzh(-x);
        return [-1, result[1], result[2], result[3]];
    }
    var negExponent = 52-Math.floor(Math.log(x) * 1.4426950408889634); // 1/log(2)
    var mantissa = goog.math.Long.fromNumber(x * Math.pow(2, negExponent));
    return [1, mantissa.getHighBits(), mantissa.getLowBits(), -negExponent];
};
var $hs_expFloatzh = function(a) {
    return Math.exp(a);
};
var $hs_logFloatzh = function(a) {
    return Math.log(a);
};
var $hs_sqrtFloatzh = function(a) {
    return Math.sqrt(a);
};
var $hs_sinFloatzh = function(a) {
    return Math.sin(a);
};
var $hs_cosFloatzh = function(a) {
    return Math.cos(a);
};
var $hs_tanFloatzh = function(a) {
    return Math.tan(a);
};
var $hs_asinFloatzh = function(a) {
    return Math.asin(a);
};
var $hs_acosFloatzh = function(a) {
    return Math.acos(a);
};
var $hs_atanFloatzh = function(a) {
    return Math.atan(a);
};
var $hs_sinhFloatzh = function(a) {
    return (Math.exp(a) - Math.exp(-a)) / 2;
};
var $hs_coshFloatzh = function(a) {
    return (Math.exp(a) + Math.exp(-a)) / 2;
};
var $hs_tanhFloatzh = function(a) {
    var expA = Math.exp(a);
    var expNegA = Math.exp(-a);
    return (expA - expNegA) / (expA + expNegA);
};
var $hs_powerFloatzh = function(a, b) {
    return Math.pow(a, b);
};
var $hs_decodeFloatzuIntzh = function(x) {
    // This probably gives unexpected results.
    // In particular we need to check...
    //   * NaN
    //   * Infinity
    if( x < 0 ) {
        var result = $hs_decodeFloatzuIntzh(-x);
        return [-result[0], result[1]];
    }
    var negExponent = 23-Math.floor(Math.log(x) * 1.4426950408889634); // 1/log(2)
    return [(x * Math.pow(2, negExponent))|0, -negExponent];
};
var isDoubleNegativeZero = function(a) {
    return (a==-0.0)?1:0;
};
var isDoubleNaN = function(a) {
    return (isNaN(a))?1:0;
};
var isDoubleInfinite = function(a) {
    return (isFinite(a)||isNaN(a))?0:1;
};
var isFloatNegativeZero = function(a) { // fixme switch to proper 32 bit floats
    return (a==-0.0)?1:0;
};
var isFloatNaN = function(a) {
    return (isNaN(a))?1:0;
};
var isFloatInfinite = function(a) {
    return (isFinite(a)||isNaN(a))?0:1;
};
var integer_cbits_encodeDouble = function(s, bits, e) {
    var g = $hs_gmpToGoog(s, bits);
    return Math.pow(2,e)*g.toNumber();
};
var __int_encodeDouble = function(b, e) {
    return Math.pow(2,e) * b;
};
var rintDouble = function(a) {
    return Math.round(a);
};
var rintFloat = function(a) {
    return Math.round(a);
};
var $hs_newMutVarzh = function(a, s) {
    return [s, {value : a}];
};
var $hs_readMutVarzh = function (a, s) {
    return [s, a.value];
};
var $hs_writeMutVarzh = function (a, b, s) {
    a.value = b;
    return s;
};
var $hs_sameMutVarzh = function (a, b) {
    return a === b;
};
var $hs_newArrayzh = function(n, a, s) {
    var result = [];
    for (var x = 0; x != n; x++)
      result[x] = a;
    return [s, result];
};
var $hs_sameMutableArrayzh = function (a, b) {
    return a === b;
};
var $hs_readArrayzh = function (a, n, s) {
    return [s, a[n]];
};
var $hs_writeArrayzh = function (a, n, b, s) {
    a[n] = b;
    return s;
};
var $hs_sizeofArrayzh = function (a, s) {
    return [s, a.length];
};
var $hs_sizeofMutableArrayzh = function (a, s) {
    return [s, a.length];
};
var $hs_indexArrayzh = function (a, n) {
    return [a[n]]; // Unboxed singleton
};
var $hs_unsafeFreezzeArrayzh = function (a, s) {
    return [s, a];
};
var $hs_unsafeThawArrayzh = function (a, s) {
    return [s, a];
};

if(WORD_SIZE_IN_BITS==32) {
    // ByteArray support
    var $hs_ptrBase = 0;

    /**
     * @param {!number} n
     * @param {Object=} s
     */
    var $hs_newByteArrayzh = function (n, s) {
        var result = [new ArrayBuffer(n), 0, $hs_ptrBase];
        result[0].ptrs=[];
        $hs_ptrBase += n;
        return [s, result];
    };
    /**
     * @param {!number} n
     * @param {Object=} s
     */
    var $hs_newPinnedByteArrayzh = function (n, s) {
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
    var $hs_newAlignedPinnedByteArrayzh = function (n, k, s) {
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
    var $hs_newByteArrayzh = function (n, s) {
        var result = [new ArrayBuffer(n.toInt()), 0, $hs_ptrBase];
        result[0].ptrs=[];
        $hs_ptrBase = $hs_ptrBase.add(n);
        return [s, result];
    };
    /**
     * @param {!goog.math.Long} n
     * @param {Object=} s
     */
    var $hs_newPinnedByteArrayzh = function (n, s) {
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
    var $hs_newAlignedPinnedByteArrayzh = function (n, k, s) {
        $hs_ptrBase = $hs_ptrBase.add(goog.math.Long.fromInt($hs_ptrBase.toInt() % k.toInt()));
        var result = [new ArrayBuffer(n.toNumber()), 0, $hs_ptrBase];
        result[0].ptrs=[];
        $hs_ptrBase = $hs_ptrBase.add(n);
        return [s, result];
    };
}

var $hs_byteArrayContentszh = function (a) {
    return a;
};
var $hs_sameMutableByteArrayzh = function (a, b) {
    return a[2] === b[2];
};
var $hs_unsafeFreezzeByteArrayzh = function (a, s) {
    return [s, a];
};
var $hs_sizeofByteArrayzh = function (a) {
    return new Uint8Array(a[0]).length;
};
var $hs_sizeofMutableByteArrayzh = function (a) {
    return new Uint8Array(a[0]).length;
};
var $hs_indexCharArrayzh = function (a, n) {
    return String.fromCharCode(new Uint8Array(a[0])[n]);
};
var $hs_indexWideCharArrayzh = function (a, n) {
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
var $hs_fastNegate = function(a) {
    if(a.negateCache_ === undefined) {
        a.negateCache_ = a.negate();
        a.negateCache_.negateCache_ = a;
    }
    return a.negateCache_;
};
var $hs_absolute = function(a) {
    if(a.isNegative()) {
        return $hs_fastNegate(a);
    }
    return a;
};
var $hs_googToGMP = function(a) {
    var bits = $hs_absolute(a).bits_;
    var s = bits.length;
    while(s !== 0 && bits[s-1] === 0)
        s--;
    s = a.isNegative() ? -s : s;
    return [WORD_SIZE_IN_BITS==32 ? s : goog.math.Long.fromNumber(s), a];
};
var $hs_gmpToGoog = function(s,bits) {
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
var $hs_indexIntArrayzh = function (a, n) {
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
var $hs_indexWordArrayzh = function (a, n) {
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
var $hs_indexAddrArrayzh = function (a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var res = a[0].ptrs[n];
    if(HS_DEBUG && res[2] !== new Uint32Array(a[0])[n])
        throw "Array Pointer Error";
    return res;
};
var $hs_indexFloatArrayzh = function (a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return new Float32Array(a[0])[n];
};
var $hs_indexDoubleArrayzh = function (a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return new Float64Array(a[0])[n];
};
var $hs_indexStablePtrArrayzh = function (a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var res = a[0].ptrs[n];
    if(HS_DEBUG && res[2] !== new Uint32Array(a[0])[n])
        throw "Array Pointer Error";
    return res;
};
var $hs_indexInt8Arrayzh = function (a, n) {
    if(WORD_SIZE_IN_BITS == 32) {
        return new Int8Array(a[0])[n];
    } else {
        return goog.math.Long.fromNumber(new Int8Array(a[0])[n.toNumber()]);
    }
};
var $hs_indexInt16Arrayzh = function (a, n) {
    if(WORD_SIZE_IN_BITS == 32) {
        return new Int16Array(a[0])[n];
    } else {
        return goog.math.Long.fromNumber(new Int16Array(a[0])[n.toNumber()]);
    }
};
var $hs_indexInt32Arrayzh = function (a, n) {
    if(WORD_SIZE_IN_BITS == 32) {
        return new Int32Array(a[0])[n];
    } else {
        return goog.math.Long.fromNumber(new Int32Array(a[0])[n.toNumber()]);
    }
};
var $hs_indexInt64Arrayzh = function (a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], n<<3);
    return goog.math.Long.fromBits(x[0], x[1]);
};
var $hs_indexWord8Arrayzh = function (a, n) {
    if(WORD_SIZE_IN_BITS == 32) {
        return new Uint8Array(a[0])[n];
    } else {
        return goog.math.Long.fromBits(new Uint8Array(a[0])[n.toNumber()], 0);
    }
};
var $hs_indexWord16Arrayzh = function (a, n) {
    if(WORD_SIZE_IN_BITS == 32) {
        return new Uint16Array(a[0])[n];
    } else {
        return goog.math.Long.fromBits(new Uint16Array(a[0])[n.toNumber()], 0);
    }
};
var $hs_indexWord32Arrayzh = function (a, n) {
    if(WORD_SIZE_IN_BITS == 32) {
        return new Uint32Array(a[0])[n];
    } else {
        return goog.math.Long.fromBits(new Uint32Array(a[0])[n.toNumber()], 0);
    }
};
var $hs_indexWord64Arrayzh = function (a, n) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], n<<3);
    return goog.math.Long.fromBits(x[0], x[1]);
};
var $hs_readCharArrayzh = function (a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [s, String.fromCharCode(new Uint8Array(a[0])[n])];
};
var $hs_readWideCharArrayzh = function (a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [s, String.fromCharCode(new Uint32Array(a[0])[n])];
};
var $hs_readIntArrayzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Int32Array(a[0])[n]];
    }
    else {
        var x = new Int32Array(a[0], n.toNumber()<<3);
        return [s, goog.math.Long.fromBits(x[0], x[1])];
    }
};
var $hs_readWordArrayzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Uint32Array(a[0])[n]];
    }
    else {
        var x = new Int32Array(a[0], n.toNumber()<<3);
        return [s, goog.math.Long.fromBits(x[0], x[1])];
    }
};
var $hs_readAddrArrayzh = function (a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var res = a[0].ptrs[n];
    if(HS_DEBUG && res[2] !== new Uint32Array(a[0])[n])
        throw "Array Pointer Error";
    return [s, res];
};
var $hs_readFloatArrayzh = function (a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [s, new Float32Array(a[0])[n]];
};
var $hs_readDoubleArrayzh = function (a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    return [s, new Float64Array(a[0])[n]];
};
var $hs_readStablePtrArrayzh = function (a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var res = a[0].ptrs[n];
    if(HS_DEBUG && res[2] !== new Uint32Array(a[0])[n])
        throw "Array Pointer Error";
    return [s, res];
};
var $hs_readInt8Arrayzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS == 32) {
        return [s, new Int8Array(a[0])[n]];
    } else {
        return [s, goog.math.Long.fromNumber(new Int8Array(a[0])[n.toNumber()])];
    }
};
var $hs_readInt16Arrayzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS == 32) {
        return [s, new Int16Array(a[0])[n]];
    } else {
        return [s, goog.math.Long.fromNumber(new Int16Array(a[0])[n.toNumber()])];
    }
};
var $hs_readInt32Arrayzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS == 32) {
        return [s, new Int32Array(a[0])[n]];
    } else {
        return [s, goog.math.Long.fromNumber(new Int32Array(a[0])[n.toNumber()])];
    }
};
var $hs_readInt64Arrayzh = function (a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], n<<3);
    return [s, goog.math.Long.fromBits(x[0], x[1])];
};
var $hs_readWord8Arrayzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS == 32) {
        return [s, new Uint8Array(a[0])[n]];
    } else {
        return [s, goog.math.Long.fromBits(new Uint8Array(a[0])[n.toNumber()], 0)];
    }
};
var $hs_readWord16Arrayzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS == 32) {
        return [s, new Uint16Array(a[0])[n]];
    } else {
        return [s, goog.math.Long.fromBits(new Uint16Array(a[0])[n.toNumber()], 0)];
    }
};
var $hs_readWord32Arrayzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS == 32) {
        return [s, new Uint32Array(a[0])[n]];
    } else {
        return [s, goog.math.Long.fromBits(new Uint32Array(a[0])[n.toNumber()], 0)];
    }
};
var $hs_readWord64Arrayzh = function (a, n, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], n<<3);
    return [s, goog.math.Long.fromBits(x[0], x[1])];
};
var $hs_writeCharArrayzh = function (a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    new Uint8Array(a[0])[n] = v.charCodeAt();
    return s;
};
var $hs_writeWideCharArrayzh = function (a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    new Uint32Array(a[0])[n] = v.charCodeAt();
    return s;
};
var $hs_writeIntArrayzh = function (a, n, v, s) {
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
var $hs_writeWordArrayzh = function (a, n, v, s) {
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
var $hs_writeAddrArrayzh = function (a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    a[0].ptrs[n] = v;
    new Uint32Array(a[0])[n] = v[2];
    return s;
};
var $hs_writeFloatArrayzh = function (a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    new Float32Array(a[0])[n] = v;
    return s;
};
var $hs_writeDoubleArrayzh = function (a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    new Float64Array(a[0])[n] = v;
    return s;
};
var $hs_writeStablePtrArrayzh = function (a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    a[0].ptrs[n] = v;
    new Uint32Array(a[0])[n] = v[2];
    return s;
};
var $hs_writeInt8Arrayzh = function (a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Int8Array(a[0])[n] = v;
    }
    else {
        new Int8Array(a[0])[n.toNumber()] = v.getLowBits();
    }
    return s;
};
var $hs_writeInt16Arrayzh = function (a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Int16Array(a[0])[n] = v;
    }
    else {
        new In16Array(a[0])[n.toNumber()] = v.getLowBits();
    }
    return s;
};
var $hs_writeInt32Arrayzh = function (a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Int32Array(a[0])[n] = v;
    }
    else {
        new Int32Array(a[0])[n.toNumber()] = v.getLowBits();
    }
    return s;
};
var $hs_writeInt64Arrayzh = function (a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], n<<3);
    x[0] = v.getLowBits();
    x[1] = v.getHighBits();
    return s;
};
var $hs_writeWord8Arrayzh = function (a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Uint8Array(a[0])[n] = v;
    }
    else {
        new Uint8Array(a[0])[n.toNumber()] = v.getLowBits();
    }
    return s;
};
var $hs_writeWord16Arrayzh = function (a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Uint16Array(a[0])[n] = v;
    }
    else {
        new Uint16Array(a[0])[n.toNumber()] = v.getLowBits();
    }
    return s;
};
var $hs_writeWord32Arrayzh = function (a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Uint32Array(a[0])[n] = v;
    }
    else {
        new Uint32Array(a[0])[n.toNumber()] = v.getLowBits();
    }
    return s;
};
var $hs_writeWord64Arrayzh = function (a, n, v, s) {
    n = WORD_SIZE_IN_BITS==32 ? n : n.toNumber();
    var x = new Int32Array(a[0], n<<3);
    x[0] = v.getLowBits();
    x[1] = v.getHighBits();
    return s;
};
var $hs_copyByteArrayzh = function (src, soff, dest, doff, count, s) {
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
var $hs_copyMutableByteArrayzh = function (src, soff, dest, doff, count, s) {
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
var $hs_plusAddrzh = function (a, n) {
    if(typeof(a) === 'string')
        return [a, n, n];
    else
        return [a[0],a[1]+n,a[2]+n];
};
var $hs_minusAddrzh = function (a, b) {
    return a[1]-b[1];
};
var $hs_remAddrzh = function (a, b) {
    return a[1]%b;
};
var $hs_gtAddrzh = function (a, b) {
    return $hs_mkBool(
         (a===null&&b===null)?false:
        ((a===null&&b!==null)?false:
        ((a!==null&&b===null)?true:(a[2]>b[2]))));
};
var $hs_geAddrzh = function (a, b) {
    return $hs_mkBool(
         (a===null&&b===null)?true:
        ((a===null&&b!==null)?false:
        ((a!==null&&b===null)?true:(a[2]>=b[2]))));
};
var $hs_eqAddrzh = function (a, b) {
    return $hs_mkBool(
         (a===null&&b===null)?true:
        ((a===null&&b!==null)?false:
        ((a!==null&&b===null)?false:(a[2]===b[2]))));
};
var $hs_neAddrzh = function (a, b) {
    return $hs_mkBool(
         (a===null&&b===null)?false:
        ((a===null&&b!==null)?true:
        ((a!==null&&b===null)?true:(a[2]!==b[2]))));
};
var $hs_ltAddrzh = function (a, b) {
    return $hs_mkBool(
         (a===null&&b===null)?false:
        ((a===null&&b!==null)?true:
        ((a!==null&&b===null)?false:(a[2]<b[2]))));
};
var $hs_leAddrzh = function (a, b) {
    return $hs_mkBool(
         (a===null&&b===null)?true:
        ((a===null&&b!==null)?true:
        ((a!==null&&b===null)?false:(a[2]<=b[2]))));
};
var $hs_indexCharOffAddrzh = function (a, n) {
    if(typeof(a) === 'string')
        return n==a.length?'\x00':a.charAt(n);
    else if(typeof(a[0]) === 'string')
        return n==a[0].length?'\x00':a[0].charAt(a[1]+n);
    else
        return String.fromCharCode(new Uint8Array(a[0],a[1]+n)[0]);
};
var $hs_indexWideCharOffAddrzh = function (a, n) {
    return String.fromCharCode(new Uint32Array(a[0],a[1]+(n<<2))[0]);
};
var $hs_indexIntOffAddrzh = function (a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Int32Array(a[0],a[1]+(n<<2))[0];
    }
    else {
        var x = new Int32Array(a[0], a[1] + (n<<3));
        return goog.math.Long.fromBits(x[0], x[1]);
    }
};
var $hs_indexWordOffAddrzh = function (a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Uint32Array(a[0],a[1]+(n<<2))[0];
    }
    else {
        var x = new Int32Array(a[0], a[1] + (n<<3));
        return goog.math.Long.fromBits(x[0], x[1]);
    }
};
var $hs_indexAddrOffAddrzh = function (a, n) {
    var res = a[0].ptrs[a[1]+(n<<2)];
    if(HS_DEBUG && res[2] !== new Uint32Array(a[0],a[1]+(n<<2))[0])
        throw "Array Pointer Error";
    return res;
};
var $hs_indexFloatOffAddrzh = function (a, n) {
    return new Float32Array(a[0],a[1]+(n<<2))[0];
};
var $hs_indexDoubleOffAddrzh = function (a, n) {
    return new Float64Array(a[0],a[1]+(n<<3))[0];
};
var $hs_indexStablePtrOffAddrzh = function (a, n) {
    var res = a[0].ptrs[a[1]+(n<<2)];
    if(HS_DEBUG && res[2] !== new Uint32Array(a[0],a[1]+(n<<2))[0])
        throw "Array Pointer Error";
    return res;
};
var $hs_indexInt8OffAddrzh = function (a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Int8Array(a[0],a[1]+n)[0];
    }
    else {
        return goog.math.Long.fromNumber(new Int8Array(a[0],a[1]+n)[0]);
    }
};
var $hs_indexInt16OffAddrzh = function (a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Int16Array(a[0],a[1]+(n<<1))[0];
    }
    else {
        return goog.math.Long.fromNumber(new Int16Array(a[0],a[1]+(n<<1))[0]);
    }
};
var $hs_indexInt32OffAddrzh = function (a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Int32Array(a[0],a[1]+(n<<2))[0];
    }
    else {
        return goog.math.Long.fromNumber(new Int32Array(a[0],a[1]+(n<<2))[0]);
    }
};
var $hs_indexInt64OffAddrzh = function (a, n) {
    var x = new Uint32Array(a[0],a[1]+(n<<3));
    return goog.math.Long.fromBits(x[0], x[1]);
};
var $hs_indexWord8OffAddrzh = function (a, n) {
    if(typeof(a) === 'string')
        var res = n==a.length?0:a.charCodeAt(n);
    else if(typeof(a[0]) === 'string')
        var res = n==a[0].length?0:a[0].charCodeAt(a[1]+n);
    else
        var res = new Uint8Array(a[0],a[1]+n)[0];
    return WORD_SIZE_IN_BITS==32 ? res : goog.math.Long.fromBits(res);
};
var $hs_indexWord16OffAddrzh = function (a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Uint16Array(a[0],a[1]+(n<<1))[0];
    }
    else {
        return goog.math.Long.fromBits(new Uint16Array(a[0],a[1]+(n<<1))[0], 0);
    }
};
var $hs_indexWord32OffAddrzh = function (a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Uint32Array(a[0],a[1]+(n<<2))[0];
    }
    else {
        return goog.math.Long.fromBits(new Uint32Array(a[0],a[1]+(n<<2))[0], 0);
    }
};
var $hs_indexWord64OffAddrzh = function (a, n) {
    var x = new Int32Array(a[0],a[1]+(n<<3));
    return goog.math.Long.fromBits(x[0], x[1]);
};
var $hs_readCharOffAddrzh = function (a, n, s) {
    return [s, String.fromCharCode(new Uint8Array(a[0],a[1]+n)[0])];
};
var $hs_readWideCharOffAddrzh = function (a, n, s) {
    return [s, String.fromCharCode(new Uint32Array(a[0],a[1]+(n<<2))[0])];
};
var $hs_readIntOffAddrzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Int32Array(a[0],a[1]+(n<<2))[0]];
    }
    else {
        var x = new Int32Array(a[0], a[1] + (n<<3));
        return [s, goog.math.Long.fromBits(x[0], x[1])];
    }
};
var $hs_readWordOffAddrzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Uint32Array(a[0],a[1]+(n<<2))[0]];
    }
    else {
        var x = new Int32Array(a[0], a[1] + (n<<3));
        return [s, goog.math.Long.fromBits(x[0], x[1])];
    }
};
var $hs_readAddrOffAddrzh = function (a, n, s) {
    var res = a[0].ptrs[a[1]+(n<<2)];
    if(HS_DEBUG && a[2] !== new Uint32Array(a[0],a[1]+(n<<2))[0])
        throw "Array Pointer Error";
    return [s, res];
};
var $hs_readFloatOffAddrzh = function (a, n, s) {
    return [s, new Float32Array(a[0],a[1]+(n<<2))[0]];
};
var $hs_readDoubleOffAddrzh = function (a, n, s) {
    return [s, new Float64Array(a[0],a[1]+(n<<3))[0]];
};
var $hs_readStablePtrOffAddrzh = function (a, n, s) {
    var res = a[0].ptrs[a[1]+(n<<2)];
    if(HS_DEBUG && a[2] !== new Uint32Array(a[0],a[1]+(n<<2))[0])
        throw "Array Pointer Error";
    return [s, res];
};
var $hs_readInt8OffAddrzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Int8Array(a[0],a[1]+n)[0]];
    }
    else {
        return [s, goog.math.Long.fromNumber(new Int32Array(a[0],a[1]+n)[0])];
    }
};
var $hs_readInt16OffAddrzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Int16Array(a[0],a[1]+(n<<1))[0]];
    }
    else {
        return [s, goog.math.Long.fromNumber(new Int16Array(a[0],a[1]+(n<<1))[0])];
    }
};
var $hs_readInt32OffAddrzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Int32Array(a[0],a[1]+(n<<2))[0]];
    }
    else {
        return [s, goog.math.Long.fromNumber(new Int32Array(a[0],a[1]+(n<<2))[0])];
    }
};
var $hs_readInt64OffAddrzh = function (a, n, s) {
    var x = new Uint32Array(a[0],a[1]+(n<<3));
    return [s, goog.math.Long.fromBits(x[0], x[1])];
};
var $hs_readWord8OffAddrzh = function (a, n, s) {
    if(typeof(a) === 'string')
        return [s, a.charCodeAt(n)];
    else
        return [s, new Uint8Array(a[0],a[1]+n)[0]];
};
var $hs_readWord16OffAddrzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Uint16Array(a[0],a[1]+(n<<1))[0]];
    }
    else {
        return [s, goog.math.Long.fromBits(new Uint16Array(a[0],a[1]+(n<<1))[0], 0)];
    }
};
var $hs_readWord32OffAddrzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Uint32Array(a[0],a[1]+(n<<2))[0]];
    }
    else {
        return [s, goog.math.Long.fromBits(new Uint32Array(a[0],a[1]+(n<<2))[0], 0)];
    }
};
var $hs_readWord64OffAddrzh = function (a, n, s) {
    var x = new Uint32Array(a[0],a[1]+(n<<3));
    return [s, goog.math.Long.fromBits(x[0], x[1])];
};
var $hs_writeCharOffAddrzh = function (a, n, v, s) {
    (new Uint8Array(a[0],a[1]+n))[0] = v.charCodeAt();
    return s;
};
var $hs_writeWideCharOffAddrzh = function (a, n, v, s) {
    (new Uint32Array(a[0],a[1]+(n<<2)))[0] = v.charCodeAt();
    return s;
};
var $hs_writeIntOffAddrzh = function (a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        (new Int32Array(a[0],a[1]+(n<<2)))[0] = v;
        return s;
    }
    else {
        var x = new Int32Array(a[0], a[1]+(n<<3));
        x[0] = v.getLowBits();
        x[1] = v.getHighBits();
        return s;
    }
};
var $hs_writeWordOffAddrzh = function (a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        (new Uint32Array(a[0],a[1]+(n<<2)))[0] = v;
        return s;
    }
    else {
        var x = new Int32Array(a[0], a[1]+(n<<3));
        x[0] = v.getLowBits();
        x[1] = v.getHighBits();
        return s;
    }
};
var $hs_writeAddrOffAddrzh = function (a, n, v, s) {
    a[0].ptrs[n] = v;
    (new Uint32Array(a[0],a[1]+(n<<2)))[0] = a[2];
    return s;
};
var $hs_writeFloatOffAddrzh = function (a, n, v, s) {
    (new Float32Array(a[0],a[1]+(n<<2)))[0] = v;
    return s;
};
var $hs_writeDoubleOffAddrzh = function (a, n, v, s) {
    (new Float64Array(a[0],a[1]+(n<<3)))[0] = v;
    return s;
};
var $hs_writeStablePtrOffAddrzh = function (a, n, v, s) {
    a[0].ptrs[n] = v;
    (new Uint32Array(a[0],a[1]+(n<<2)))[0] = a[2];
    return s;
};
var $hs_writeInt8OffAddrzh = function (a, n, v, s) {
    (new Int8Array(a[0],a[1]+n))[0] = WORD_SIZE_IN_BITS==32 ? v : v.getLowBits();
    return s;
};
var $hs_writeInt16OffAddrzh = function (a, n, v, s) {
    (new Int16Array(a[0],a[1]+(n<<1)))[0] = WORD_SIZE_IN_BITS==32 ? v : v.getLowBits();
    return s;
};
var $hs_writeInt32OffAddrzh = function (a, n, v, s) {
    (new Int32Array(a[0],a[1]+(n<<2)))[0] = WORD_SIZE_IN_BITS==32 ? v : v.getLowBits();
    return s;
};
var $hs_writeInt64OffAddrzh = function (a, n, v, s) {
    var x = new Int32Array(a[0], a[1]+(n<<3));
    x[0] = v.getLowBits();
    x[1] = v.getHighBits();
    return s;
};
var $hs_writeWord8OffAddrzh = function (a, n, v, s) {
    (new Uint8Array(a[0],a[1]+n))[0] = WORD_SIZE_IN_BITS==32 ? v : v.getLowBits();
    return s;
};
var $hs_writeWord16OffAddrzh = function (a, n, v, s) {
    (new Uint16Array(a[0],a[1]+(n<<1)))[0] = WORD_SIZE_IN_BITS==32 ? v : v.getLowBits();
    return s;
};
var $hs_writeWord32OffAddrzh = function (a, n, v, s) {
    (new Uint32Array(a[0],a[1]+(n<<2)))[0] = WORD_SIZE_IN_BITS==32 ? v : v.getLowBits();
    return s;
};
var $hs_writeWord64OffAddrzh = function (a, n, v, s) {
    var x = new Int32Array(a[0], a[1]+(n<<3));
    x[0] = v.getLowBits();
    x[1] = v.getHighBits();
    return s;
};
var $hs_alert = function (str) {
    window.alert(str);
};
var $hs_logAny = function (c, str) {
    var el = document.getElementById('log');
    el.innerHTML = el.innerHTML + c + ": " + str + '<br/>\n';
};
var $hs_logInfo = function (str) {
    $hs_logAny("INFO", str);
};
var $hs_logError = function (str) {
    $hs_logAny("ERROR", str);
};
var $hs_logDebug = function (str) {
    $hs_logAny("DEBUG", str);
};

var $hs_logger = goog.debug.Logger.getLogger('hs');

var $hs_utf32 = function(s) {
    var res = $hs_newByteArrayzh((s.length<<2)+4)[1];
    var dest = new Uint32Array(res[0]);
    for(var i=0;i!=s.length;i++)
        dest[i]=s.charCodeAt(i);
    dest[i]=0;
    return res;
};
var $hs_fromUtf32 = function(s) {
    var res = "";
    var src = new Uint32Array(s[0],s[1]);
    var len = src[src.length-1] === 0 ? src.length - 1 : src.length;
    for(var i=0;i!=len;i++)
        res=res+String.fromCharCode(src[i]);
    return res;
};
var $hs_ascii = function(s) {
    var res = $hs_newByteArrayzh(s.length+1)[1];
    var dest = new Uint8Array(res[0]);
    for(var i=0;i!=s.length;i++)
        dest[i]=s.charCodeAt(i);
    dest[i]=0;
    return res;
};
var integer_cmm_cmpIntegerzh = function(sa, abits, sb, bbits) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_gmpToGoog(sa, abits).compare($hs_gmpToGoog(sb, bbits));
    }
    else {
        return goog.math.Long.fromNumber($hs_gmpToGoog(sa, abits).compare(
            $hs_gmpToGoog(sb, bbits)));
    }
};
var integer_cmm_cmpIntegerIntzh = function(sa, abits, b) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_gmpToGoog(sa, abits).compare(goog.math.Integer.fromInt(b));
    }
    else {
        return goog.math.Long.fromNumber($hs_gmpToGoog(sa, abits).compare(
            goog.math.Integer.fromBits([b.getLowBits(), b.getHighBits()])));
    }
};
var integer_cmm_plusIntegerzh = function(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).add($hs_gmpToGoog(sb, bbits)));
};
var integer_cmm_minusIntegerzh = function(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).subtract($hs_gmpToGoog(sb, bbits)));
};
var integer_cmm_timesIntegerzh = function(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).multiply($hs_gmpToGoog(sb, bbits)));
};
var integer_cmm_quotRemIntegerzh = function(sa, abits, sb, bbits) {
    var a = $hs_gmpToGoog(sa, abits);
    var b = $hs_gmpToGoog(sb, bbits)
    var q = a.divide(b);
    var r = a.subtract(q.multiply(b));
    return $hs_googToGMP(q).concat($hs_googToGMP(r));
};
var integer_cmm_quotIntegerzh = function(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).divide($hs_gmpToGoog(sb, bbits)));
};
var integer_cmm_remIntegerzh = function(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).modulo($hs_gmpToGoog(sb, bbits)));
};
var integer_cmm_divModIntegerzh = function(sa, abits, sb, bbits) {
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
var integer_cmm_divExactIntegerzh = function(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).divide($hs_gmpToGoog(sb, bbits)));
};
var $hs_gcd = function(a, b) {
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
var integer_cmm_gcdIntegerzh = function(sa, abits, sb, bbits) {
    return $hs_gcd($hs_gmpToGoog(sa, abits), $hs_gmpToGoog(sb, bbits));
};
var integer_cmm_gcdIntegerIntzh = function(sa, abits, b) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_gcd($hs_gmpToGoog(sa, abits), goog.math.Integer.fromInt(b));
    }
    else {
        return $hs_gcd($hs_gmpToGoog(sa, abits),
            goog.math.Integer.fromBits([b.getLowBits(), b.getHighBits()]));
    }
};
var integer_cmm_gcdIntzh = function(a, b) {
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
            var q = big.divide(small);
            var r = big.subtract(q.multiply(small));
            big = small;
            small = r;
        }
        return big;
    }
};
var integer_cmm_decodeDoublezh = function(x) {
    if( x < 0 ) {
        var result = integer_cmm_decodeDoublezh(-x);
        return [result[0], -result[1], result[2]];
    }
    var negExponent = 52-Math.floor(Math.log(x) * 1.4426950408889634); // 1/log(2)
    return [-negExponent].concat($hs_googToGMP(
        goog.math.Integer.fromNumber(x * Math.pow(2, negExponent))));
};
var integer_cmm_int2Integerzh = function(i) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_googToGMP(goog.math.Integer.fromInt(i));
    }
    else {
        return $hs_googToGMP(
            goog.math.Integer.fromBits([i.getLowBits(), i.getHighBits()]));
    }
};
var integer_cmm_word2Integerzh = function(i) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs_googToGMP(goog.math.Integer.fromBits(i<0?[0,i]:[i]));
    }
    else {
        return $hs_googToGMP(goog.math.Integer.fromBits(
            i.isNegative()?[0, i.getLowBits(), i.getHighBits()]
                          :[   i.getLowBits(), i.getHighBits()]));
    }
};
var integer_cmm_andIntegerzh = function(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).and($hs_gmpToGoog(sb, bbits)));
};
var integer_cmm_orIntegerzh = function(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).or($hs_gmpToGoog(sb, bbits)));
};
var integer_cmm_xorIntegerzh = function(sa, abits, sb, bbits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).xor($hs_gmpToGoog(sb, bbits)));
};
var integer_cmm_mul2ExpIntegerzh = function(sa, abits, b) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).shiftLeft(b));
};
var integer_cmm_fdivQ2ExpIntegerzh = function(sa, abits, b) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).shiftRight(b));
};
var integer_cmm_complementIntegerzh = function(sa, abits) {
    return $hs_googToGMP($hs_gmpToGoog(sa, abits).not());
};
var integer_cmm_int64ToIntegerzh = function(a) {
    return $hs_googToGMP(goog.math.Integer.fromBits([a.getLowBits(), a.getHighBits()]));
};
var integer_cmm_word64ToIntegerzh = function(a) {
    return $hs_googToGMP(goog.math.Integer.fromBits(
            a.isNegative()?[0, a.getLowBits(), a.getHighBits()]
                          :[   a.getLowBits(), a.getHighBits()]));
};
var hs_integerToInt64 = function(as, abits) {
    var a = $hs_gmpToGoog(as, abits);
    return goog.math.Long.fromBits(a.getBits(0), a.getBits(1));
};
var hs_integerToWord64 = function(as, abits) {
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
var _hs_text_memcpy = function (dest, doff, src, soff, count) {
    var srcarray = new Uint16Array(src[0],src[1]);
    var destarray = new Uint16Array(dest[0],dest[1]);
    while(count != 0) {
        destarray[doff] = srcarray[soff];
        soff++;
        doff++;
        count--;
    }
};
var _hs_text_memcmp = function(a, aoff, b, boff, count) {
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

var $hs_decode = function(state, cp, b) {
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
var _hs_text_decode_utf8 = function(dest, doffptr, src, srcend)
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
        if ($hs_decode(state, codepoint, srcarray[s++]) != $hs_UTF8_ACCEPT) {
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
var memcpy = function(dest, src, count) {
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
var memcmp = function(a, b, count) {
    if(typeof(a) === 'string') {
        if(typeof(b) === 'string') {
            var x = a.slice(0,count).localeCompare(b.slice(0,count));
            return x === 0 ? 0 : (x < 0 ? -1 : 1);
        }
        var barray = new Uint8Array(b[0],b[1]);
        var aoff = 0;
        var boff = 0;
        while(count != 0) {
            if( a[aoff] < barray[boff] )
                return -1;
            if( a[aoff] > barray[boff] )
                return 1;
            aoff++;
            boff++;
            count--;
        }
        return 0;
    }
    else {
        if(typeof(b) === 'string') {
            var aarray = new Uint8Array(a[0],a[1]);
            var aoff = 0;
            var boff = 0;
            while(count != 0) {
                if( aarray[aoff] < b[boff] )
                    return -1;
                if( aarray[aoff] > b[boff] )
                    return 1;
                aoff++;
                boff++;
                count--;
            }
            return 0;
        }
        var aarray = new Uint8Array(a[0],a[1]);
        var barray = new Uint8Array(b[0],b[1]);
        var aoff = 0;
        var boff = 0;
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
    }
};

var $hs_unicodeCat = null;

var u_gencat = function(a) {
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
    return $hs_unicodeCat[a];
};
var u_iswalpha = function(a) {
    return goog.string.isAlpha(String.fromCharCode(a)) ?
        (WORD_SIZE_IN_BITS == 32 ? 1 : goog.math.Long.ONE)
       :(WORD_SIZE_IN_BITS == 32 ? 0 : goog.math.Long.ZERO);
};
var u_iswalnum = function(a) {
    return goog.string.isAlphaNumeric(String.fromCharCode(a)) ?
        (WORD_SIZE_IN_BITS == 32 ? 1 : goog.math.Long.ONE)
       :(WORD_SIZE_IN_BITS == 32 ? 0 : goog.math.Long.ZERO);
};
var u_iswspace = function(a) {
    return '\t\n\v\f\r \u0020\u00a0\u1680\u180e\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u202f\u205f\u3000'
        .indexOf(String.fromCharCode(a)) != -1 ?
        (WORD_SIZE_IN_BITS == 32 ? 1 : goog.math.Long.ONE)
       :(WORD_SIZE_IN_BITS == 32 ? 0 : goog.math.Long.ZERO);
};
var u_iswlower = function(a) {
    return a !== u_towupper(a) ?
        (WORD_SIZE_IN_BITS == 32 ? 1 : goog.math.Long.ONE)
       :(WORD_SIZE_IN_BITS == 32 ? 0 : goog.math.Long.ZERO);
};
var u_iswupper = function(a) {
    return a !== u_towlower(a) ?
        (WORD_SIZE_IN_BITS == 32 ? 1 : goog.math.Long.ONE)
       :(WORD_SIZE_IN_BITS == 32 ? 0 : goog.math.Long.ZERO);
};
var u_towlower = function(a) { return String.fromCharCode(a).toLowerCase().charCodeAt(); };
var u_towupper = function(a) { return String.fromCharCode(a).toUpperCase().charCodeAt(); };
var rtsSupportsBoundThreads = function () { return 0; };
var getOrSetGHCConcSignalSignalHandlerStore = function (x) { return x; };
var stg_sig_install = function(a,b,c) { return -1; };
var localeEncoding = function() { return $hs_ascii("UTF-32LE"); };
var hs_iconv_open = function(to,from) { return 1; };
var hs_iconv_close = function(h) { return 0; };

var $hs_unsignedCompare = function(a,b) {
    if (a.equals(b)) return 0;

    var aneg = a.isNegative();
    var bneg = b.isNegative();
    if (aneg && !bneg) return 1;
    if (!aneg && bneg) return -1;

    return a.subtract(b).isNegative() ? -1 : 1;
};


if(WORD_SIZE_IN_BITS == 32) {
    // Safer 32 bit multiplication than just a * b
    var $hs_timesIntzh = function(a, b) {
       return goog.math.Long(a,0).multiply(goog.math.Long(b,0)).getLowBits(); };
}

if(WORD_SIZE_IN_BITS == 64) {
    // Int primatives for 64bit
    var $hs_quotIntzh = function(a, b) {
       return a.div(b); };
    var $hs_remIntzh = function(a, b) {
       return a.modulo(b); };
    var $hs_int2Wordzh = function(a) {
       return a; };
    var $hs_int2Floatzh = function(a) {
       return a.toNumber(); };
    var $hs_int2Doublezh = function(a) {
       return a.toNumber(); };
    var $hs_uncheckedIShiftLzh = function(a, b) {
       return a.shiftLeft(b.toNumber()); };
    var $hs_uncheckedIShiftRAzh = function(a, b) {
       return a.shiftRight(b.toNumber()); };
    var $hs_uncheckedIShiftRLzh = function(a, b) {
       return a.shiftRight(b.toNumber()); };

    // Word primatives for 64bit
    var $hs_quotWordzh = function(a, b) {
       return a.div(b); };   // TODO make unsigned
    var $hs_remWordzh = function(a, b) {
       return a.modulo(b); }; // TODO make unsigned
    var $hs_uncheckedShiftLzh = function(a, b) {
       return a.shiftLeft(b.toNumber()); };
    var $hs_uncheckedShiftRLzh = function(a, b) {
       return a.shiftRight(b.toNumber()); };
    var $hs_word2Intzh = function(a) {
       return a; };
    var $hs_gtWordzh = function(a, b) {
       return $hs_mkBool($hs_unsignedCompare(a, b) > 0); };
    var $hs_geWordzh = function(a, b) {
       return $hs_mkBool($hs_unsignedCompare(a, b) >= 0); };
    var $hs_eqWordzh = function(a, b) {
       return $hs_mkBool(a.equals(b)); };
    var $hs_neWordzh = function(a, b) {
       return $hs_mkBool(a.notEquals(b)); };
    var $hs_ltWordzh = function(a, b) {
       return $hs_mkBool($hs_unsignedCompare(a, b) < 0); };
    var $hs_leWordzh = function(a, b) {
       return $hs_mkBool($hs_unsignedCompare(a, b) <= 0); };

    var $hs_intToIntzh = function(i) {
       return goog.math.Long.fromInt(i); };
    var $hs_intzhToInt = function(i) {
       return i.toInt(); };
    var $hs_int64ToWordzh = function(i) {
       return i; };
    var $hs_wordToWordzh = function(i) {
       return goog.math.Long.fromBits(i,0); };
    var $hs_wordzhToWord = function(i) {
       return i.getLowBits(); };
    var $hs_word64ToIntzh = function(i) {
       return i; };

    var $hs_narrow8Intzh = function(i) {
       low = i.getLowBits();
       return goog.math.Long.fromNumber((low & 0x7F) - (low & 0x80)); };
    var $hs_narrow16Intzh = function(i) {
       low = i.getLowBits();
       return goog.math.Long.fromNumber((low & 0x7FFF) - (low & 0x8000)); };
    var $hs_narrow32Intzh = function(i) {
       return goog.math.Long.fromNumber(i.getLowBits()|0); };
    var $hs_narrow8Wordzh = function(i) {
       return goog.math.Long.fromBits(i.getLowBits() & 0xFF, 0); };
    var $hs_narrow16Wordzh = function(i) {
       return goog.math.Long.fromBits(i.getLowBits() & 0xFFFF, 0); };
    var $hs_narrow32Wordzh = function(i) {
       return goog.math.Long.fromBits(i.getLowBits(), 0); };
}

var hs_gtWord64 = function(a, b) {
   return $hs_unsignedCompare(a, b) > 0 ? 1 : 0; };
var hs_geWord64 = function(a, b) {
   return $hs_unsignedCompare(a, b) >= 0 ? 1 : 0; };
var hs_eqWord64 = function(a, b) {
   return a.equals(b) ? 1 : 0; };
var hs_neWord64 = function(a, b) {
   return a.notEquals(b) ? 1 : 0; };
var hs_ltWord64 = function(a, b) {
   return $hs_unsignedCompare(a, b) < 0 ? 1 : 0; };
var hs_leWord64 = function(a, b) {
   return $hs_unsignedCompare(a, b) <= 0 ? 1 : 0; };

var hs_gtInt64 = function(a, b) {
   return a.greaterThan(b) ? 1 : 0; };
var hs_geInt64 = function(a, b) {
   return a.greaterThanOrEqual(b) ? 1 : 0; };
var hs_eqInt64 = function(a, b) {
   return a.equals(b) ? 1 : 0; };
var hs_neInt64 = function(a, b) {
   return a.notEquals(b) ? 1 : 0; };
var hs_ltInt64 = function(a, b) {
   return a.lessThan(b) ? 1 : 0; };
var hs_leInt64 = function(a, b) {
   return a.lessThanOrEqual(b) ? 1 : 0; };

var hs_remWord64 = function(a, b) {
   return a.modulo(b); }; // TODO make unsigned
var hs_quotWord64 = function(a, b) {
   return a.div(b); };   // TODO make unsigned

var hs_remInt64 = function(a, b) {
   return a.modulo(b); };
var hs_quotInt64 = function(a, b) {
   return a.div(b); };
var hs_negateInt64 = function(a) {
   return a.negate(); };
var hs_plusInt64 = function(a, b) {
   return a.add(b); };
var hs_minusInt64 = function(a, b) {
   return a.subtract(b); };
var hs_timesInt64 = function(a, b) {
   return a.multiply(b); };

var hs_and64 = function(a, b) {
   return a.and(b); };
var hs_or64 = function(a, b) {
   return a.or(b); };
var hs_xor64 = function(a, b) {
   return a.xor(b); };
var hs_not64 = function(a) {
   return a.not(); };

var hs_uncheckedShiftL64 = function(a, b) {
   return a.shiftLeft(b); };
var hs_uncheckedShiftRL64 = function(a, b) {
   return a.shiftRight(b); };
var hs_uncheckedIShiftL64 = function(a, b) {
   return a.shiftLeft(b); };
var hs_uncheckedIShiftRA64 = function(a, b) {
   return a.shiftRight(b); };
var hs_uncheckedIShiftRL64 = function(a, b) {
   return a.shiftRight(b); };

var hs_intToInt64 = function(i) {
   return goog.math.Long.fromInt(i); };
var hs_int64ToInt = function(i) {
   return i.toInt(); };
var hs_int64ToWord64 = function(i) {
   return i; };
var hs_wordToWord64 = function(i) {
   return goog.math.Long.fromBits(i,0); };
var hs_word64ToWord = function(i) {
   return i.getLowBits(); };
var hs_word64ToInt64 = function(i) {
   return i; };

var errno = 0;
var __hscore_get_errno = function() {
    HS_RTS_TRACE && $hs_logger.info('__hscore_get_errno');
    return errno;
}
var __hscore_set_errno = function(e) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_set_errno');
    errno = e;
};
var strerror = function(e) {
    return $hs_utf32("Error "+e);
}
var __hscore_s_isreg = function(m) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_s_isreg');
    return 1;
};
var __hscore_s_isdir = function(m) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_s_isdir');
    return 0;
};
var __hscore_s_isfifo = function(m) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_s_isfifo');
    return 0;
};
var __hscore_s_isblk = function(m) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_s_isblk');
    return 0;
};
var __hscore_s_ischr = function(m) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_s_ischr');
    return 0;
};
var __hscore_s_issock = function(m) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_s_issock');
    return 0;
};

var __hscore_sigemptyset = function(set) { return 0; };
var __hscore_sigfillset = function(set) { return 0; };
var __hscore_sigaddset = function(set, s) { return 0; };
var __hscore_sigdelset = function(set, s) { return 0; };
var __hscore_sigismember = function(set, s) { return 0; };

var __hscore_memcpy_src_off = function(dest, src, soff, count) {
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
var __hscore_bufsiz = function() { return 1024; };
var __hscore_seek_cur = function() { return 1; };
var __hscore_seek_set = function() { return 0; };
var __hscore_seek_end = function() { return 2; };

var __hscore_o_binary = function() { return 0; };
var __hscore_o_rdonly = function() { return 0; };
var __hscore_o_wronly = function() { return 0x0001; };
var __hscore_o_rdwr = function() { return 0x0002; };
var __hscore_o_append = function() { return 0x0008; };
var __hscore_o_creat = function() { return 0x0200; };
var __hscore_o_excl = function() { return 0x0800; };
var __hscore_o_trunc = function() { return 0x0400; };
var __hscore_o_noctty = function() { return 0x20000; };
var __hscore_o_nonblock = function() { return 0x0004; };

var __hscore_ftruncate = function(fd, where) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_ftruncate');
    return 0;
};
var __hscore_setmode = function(fd, toBin) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_setmode');
    return 0;
};

var __hscore_sizeof_stat = function() { return 4; };
var __hscore_st_mtime = function(st) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_st_mtime');
    return 0;
};
var __hscore_st_size = function(st) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_st_size');
    return 0;
};
var __hscore_st_mode = function(st) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_st_mode');
    return 0;
};
var __hscore_st_dev = function(st) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_st_dev');
    return 0;
};
var __hscore_st_ino = function(st) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_st_ino');
    return 0;
};
var __hscore_stat = function(f, buf) {
    var p = $hs_fromUtf32(f);
    HS_RTS_TRACE && $hs_logger.info('__hscore_stat '+p);
    return 0;
};
var __hscore_fstat = function(fd, buf) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_fstat');
    return 0;
};
var __hscore_lstat = function(f, buf) {
    var p = $hs_fromUtf32(f);
    HS_RTS_TRACE && $hs_logger.info('__hscore_lstat '+p);
    return 0;
};

var __hscore_lflag = function(ts) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_fstat');
    return ts.c_lflag;
};
var __hscore_poke_lflag = function(ts, t) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_poke_lflag');
    ts.c_lflag = t;
};
var __hscore_ptr_c_cc = function(ts) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_ptr_c_cc');
    return ts.c_cc;
};
var __hscore_sizeof_termios = function() { return 0; };
var __hscore_sizeof_sigset_t = function() { return 0; };

var __hscore_echo = function() { return 0; };
var __hscore_tcsanow = function() { return 0; };
var __hscore_icanon = function() { return 0; };
var __hscore_vmin = function() { return 0; };
var __hscore_vtime = function() { return 0; };
var __hscore_sigttou = function() { return 0; };
var __hscore_sig_block = function() { return 0; };
var __hscore_sig_setmask = function() { return 0; };
var __hscore_sizeof_siginfo_t = function() { return 0; };
var __hscore_f_getfl = function() { return 0; };
var __hscore_f_setfl = function() { return 0; };
var __hscore_f_setfd = function() { return 0; };
var __hscore_fd_cloexec = function() { return 0; };
var __hscore_get_saved_termios = function(fd) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_get_saved_termios');
    return null;
};
var __hscore_set_saved_termios = function(fd, ts) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_set_saved_termios');
};
var __hscore_hs_fileno = function(f) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_hs_fileno');
    return 0;
};
var $hs_fstab = [];
var $hs_getFileURL = function(f) {
    for(var i=0;i!==$hs_fstab.length;i++) {
        if(f.slice(0, $hs_fstab[i].mountPoint.length) === $hs_fstab[i].mountPoint)
            return $hs_fstab[i].url + f.slice($hs_fstab[i].mountPoint.length);
    }
    return null;
};
var $hs_allFiles = [
    { text:"", fptr:0 }, // stdin
    { text:"", fptr:0 }, // stdout
    { text:"", fptr:0 }  // stderr
];
var fdReady = function (fd) {
    if (fd >= $hs_allFiles.length) return -1;
    var f = $hs_allFiles[fd];
    return f.text.length > f.fptr ? 1 : 0;
};
var $hs_findFile = function(f) {
    for(var i=0;i!==$hs_allFiles.length;i++) {
        if(f===$hs_allFiles[i].path)
            return i;
    }
    return -1;
};
var __hscore_open = function(f,h,m) {
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
                    $hs_allFiles[result] = {text:transport.responseText, fptr:0, path:p};
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
                $hs_allFiles[result] = {text:"", fptr:0, path:p};
            }
        }
    }
    return result;
};
var close = function(fd) {
    HS_RTS_TRACE && $hs_logger.info('close');
    return 0;
};
var __hscore_lseek = function(fd, off, whence) {
    HS_RTS_TRACE && $hs_logger.info('__hscore_lseek');
    return 0;
};

var hsFD_SETSIZE = function() { return 1024; };
var hsFD_ISSET = function(fd, fds) {
    HS_RTS_TRACE && $hs_logger.info('hsFD_ISSET');
    return 0;
};
var hsFD_SET = function(fd, fds) {
    HS_RTS_TRACE && $hs_logger.info('hsFD_SET');
};
var sizeof_fd_set = function() {
    HS_RTS_TRACE && $hs_logger.info('sizeof_fd_set');
    return 0;
};
var hsFD_ZERO = function(fds) {
    HS_RTS_TRACE && $hs_logger.info('hsFD_ZERO');
};

var __hscore_select = function(nfds, readfds, writefds, exceptfds, timeout) {
    HS_RTS_TRACE && $hs_logger.info('hsFD_ZERO');
    return 0;
};

var environ = {};
environ['TMPDIR'] = $hs_utf32("/tmp");
var __hscore_environ = function() {
    HS_RTS_TRACE && $hs_logger.info('__hscore_environ');
    return environ;
};
var getenv = function(e) {
    var s = $hs_fromUtf32(e);
    HS_RTS_TRACE && $hs_logger.info('getenv '+s);
    var v = environ[s];
    return v===undefined?null:v;
};
var lockFile = function(fd, dev, ino, for_writing) {
    HS_RTS_TRACE && $hs_logger.info('lockFile');
    return 0;
};
var unlockFile = function(fd) {
    HS_RTS_TRACE && $hs_logger.info('unlockFile');
    return 0;
};
var isatty = function(fd) {
    HS_RTS_TRACE && $hs_logger.info('isatty');
    return 0;
};
var read = function(fd, p, s) {
    HS_RTS_TRACE && $hs_logger.info('read');
    var f = $hs_allFiles[fd];
    if (f === undefined || f === null)
        return -1;

    var n = f.text.length - f.fptr;
    if( n <= 0 ) {
        HS_RTS_TRACE && $hs_logger.info('read : end of file');
        return 0;
    }

    var maxChars = s>>>2;
    if( n > maxChars )
        n = maxChars;

    var end = f.fptr + n;
    var dest = new Uint32Array(p[0], p[1]);
    for(var i=f.fptr;i!=end;i++)
        dest[i]=f.text.charCodeAt(i);
    f.fptr=end;

    HS_RTS_TRACE && $hs_logger.info('read : '+(n<<2));
    return n<<2;
};
var write = function(fd, p, s) {
    HS_RTS_TRACE && $hs_logger.info('write');
    var f = $hs_allFiles[fd];
    if (f === undefined || f === null)
        return -1;

    var len = s>>>2;
    var src = new Uint32Array(p[0], p[1]);
    var res = "";
    for(var i=0;i!=len;i++)
        res=res+String.fromCharCode(src[i]);

    if(f.fptr <= f.text.length)
        f.text=f.text+res;
    else if(f.text.length > f.fptr + len)
        f.text=f.text.slice(0,f.fptr)+res+f.text.slice(f.fptr+len);
    else
        f.text=f.text.slice(0,f.fptr)+res+f.text.slice(f.fptr+len);

    return s;
};
var ghc_strlen = function(s) {
    return s.indexOf('\x00');
};
var initLinker = function() {
    HS_RTS_TRACE && $hs_logger.info('initLinker');
};
var exitLinker = function() {
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
var $hs_fromString = function(args, onComplete, onException) {
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
var $hs_toString = function(s) {
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
var $hs_fromText = function(args, onComplete, onException) {
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
var $hs_fromLazyText = function(args, onComplete, onException) {
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
var $hs_toText = function(s) {
    var a = $hs_newByteArrayzh((s.length << 1) + 2)[1];
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
var $hs_toLazyText = function(s) {
    var a = $hs_newByteArrayzh((s.length << 1) + 2)[1];
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
var $hs_fromInt = function(args, onComplete, onException) {
    $hs_force(args, function(i){
        if(HS_DEBUG && i.g !== 1) throw "Invalid Int"
        onComplete(WORD_SIZE_IN_BITS == 32 ? i.v[0] : i.v[0].toNumber());}, onException);
};
/**
 * @param {Array.<Object>}      args
 * @param {function(!Object)}   onComplete
 * @param {function(!Object)=}  onException
 */
var $hs_runIO = function(args, onComplete, onException) {
    var newArguments = [];
    for (var i = 0; i < args.length; i++)
        newArguments[i] = args[i];
    newArguments[args.length] = $$GHCziPrim_realWorldzh;
    $hs_force(newArguments, function(i){onComplete(i[1]);}, onException);
};
/**
 * @param {number} i
 */
var $hs_toInt = function(i) {
    return $d(1, [WORD_SIZE_IN_BITS == 32 ? i|0 : goog.math.Long.fromNumber(i)]);
};
var hs_nil = function() {
    return "";
};
var hs_cons = function(x, xs) {
    return String.fromCharCode(x) + xs;
};
var $$GHCziPrim_realWorldzh = 0;
var $$GHCziPrim_coercionTokenzh = 0;
var $hs_init = function() {
    $$GHCziTypes_False = $d(1, []);
    $$GHCziTypes_True = $d(2, []);
};
var MD5Init = function(ctx) {
    ctx.googCtx = new goog.crypt.Md5();
};
var MD5Update = function(ctx, dat, len) {
    var i8 = new Int8Array(dat[0]);
    ctx.googCtx.update(i8, len);
};
var MD5Final = function(dst, ctx) {
    var digest = ctx.googCtx.digest();
    var i8 = new Int8Array(dst);
    for(var i=0;i<16;i++) {
      i8[i] = digest[i];
    }
};
