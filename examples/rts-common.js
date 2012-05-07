var $hs = {};

$hs.mkBool = function(b) {
    return b ? $$GHCziTypes_True:$$GHCziTypes_False;
};
$hs.mulIntMayOflozh = function(a, b) {
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
$hs.addIntCzh = function(a, b) {
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
$hs.subIntCzh = function(a, b) {
    if(WORD_SIZE_IN_BITS==32) {
        var x = a - b;
        return [x|0, x===(x|0)?0:1];
    }
    else {
        var x = goog.math.Integer.fromBits([a.getLowBits(), a.getHighBits()])
            .sub(goog.math.Integer.fromBits([b.getLowBits(), b.getHighBits()]));
        return [goog.math.Long.fromBits(x.getBits(0), x.getBits(1)),
            x.sign_===x.getBits(2)?0:1];
    }
};
$hs.ordzh = function(a) {
    return a.charCodeAt(0);
};
$hs.chrzh = function(a) {
    return String.fromCharCode(a);
};
$hs.popCntTab =
   [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8];
$hs.popCnt8 = function(a) {
    return $hs.popCntTab(a & 0xFF);
};
$hs.popCnt8zh = function(a) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs.popCnt8(a);
    }
    else {
        return goog.math.Long.fromNumber(
            $hs.popCnt8(a.getLowBits()));
    }
};
$hs.popCnt16zh = function(a) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs.popCnt8(a) + $hs.popCnt8(a>>>8);
    }
    else {
        return goog.math.Long.fromNumber(
            $hs.popCnt8(a.getLowBits())+$hs.popCnt8(a.getLowBits()>>>8));
    }
};
$hs.popCnt32zh = function(a) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs.popCnt8(a) + $hs.popCnt8(a>>>8) + $hs.popCnt8(a>>>16) + $hs.popCnt8(a>>>24);
    }
    else {
        var l = a.getLowBits();
        return goog.math.Long.fromNumber(
            $hs.popCnt8(l)+$hs.popCnt8(l>>>8) + $hs.popCnt8(l>>>16) + $hs.popCnt8(l>>>24));
    }
};
$hs.popCnt64zh = function(a) {
    var l = a.getLowBits();
    var h = a.getHighBits();
    var result =
        $hs.popCnt8(l)+$hs.popCnt8(l>>>8) + $hs.popCnt8(l>>>16) + $hs.popCnt8(l>>>24) +
        $hs.popCnt8(h)+$hs.popCnt8(h>>>8) + $hs.popCnt8(h>>>16) + $hs.popCnt8(h>>>24);
    if(WORD_SIZE_IN_BITS==32) {
        return result;
    }
    else {
        return goog.math.Long.fromNumber(result);
    }
};
if(WORD_SIZE_IN_BITS==32) {
    $hs.popCntzh = $hs.popCnt32zh;
}
else {
    $hs.popCntzh = $hs.popCnt64zh;
}
$hs.newMutVarzh = function(a, s) {
    return [s, {value : a}];
};
$hs.readMutVarzh = function (a, s) {
    return [s, a.value];
};
$hs.writeMutVarzh = function (a, b, s) {
    a.value = b;
    return s;
};
$hs.sameMutVarzh = function (a, b) {
    return a === b;
};
$hs.newArrayzh = function(n, a, s) {
    var result = [];
    for (x = 0; x != n; x++)
      result[x] = a;
    return [s, result];
};
$hs.sameMutableArrayzh = function (a, b) {
    return a === b;
};
$hs.readArrayzh = function (a, n, s) {
    return [s, a[n]];
};
$hs.writeArrayzh = function (a, n, b, s) {
    a[n] = b;
    return s;
};
$hs.sizeofArrayzh = function (a, s) {
    return [s, a.length];
};
$hs.sizeofMutableArrayzh = function (a, s) {
    return [s, a.length];
};
$hs.indexArrayzh = function (a, n) {
    return a[n];
};
$hs.unsafeFreezeArrayzh = function (a, s) {
    return [s, a];
};
$hs.unsafeThawArrayzh = function (a, s) {
    return [s, a];
};

// ByteArray support
$hs.ptrBase = 0;

$hs.newByteArrayzh = function (n, s) {
    var result = [new ArrayBuffer(n), 0, $hs.ptrBase];
    result[0].ptrs=[];
    $hs.ptrBase += n;
    return [s, result];
};
$hs.newPinnedByteArrayzh = function (n, s) {
    var result = [new ArrayBuffer(n), 0, $hs.ptrBase];
    result[0].ptrs=[];
    $hs.ptrBase += n;
    return [s, result];
};
$hs.newAlignedPinnedByteArrayzh = function (n, k, s) {
    $hs.ptrBase += $hs.ptrBase%k;
    var result = [new ArrayBuffer(n), 0, $hs.ptrBase];
    result[0].ptrs=[];
    $hs.ptrBase += n;
    return [s, result];
};
$hs.byteArrayContentszh = function (a) {
    return a;
};
$hs.sameMutableByteArrayzh = function (a, b) {
    return a[2] === b[2];
};
$hs.unsafeFreezzeByteArrayzh = function (a, s) {
    return [s, a];
};
$hs.sizeofByteArrayzh = function (a) {
    return new Uint8Array(a[0]).length;
};
$hs.sizeofMutableByteArrayzh = function (a) {
    return new Uint8Array(a[0]).length;
};
$hs.indexCharArrayzh = function (a, n) {
    return String.fromCharCode(new Uint8Array(a[0])[n]);
};
$hs.indexWideCharArrayzh = function (a, n) {
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
$hs.fastNegate = function(a) {
    if(a.negateCache_ === undefined) {
        a.negateCache_ = a.negate();
        a.negateCache_.negateCache_ = a;
    }
    return a.negateCache_;
};
$hs.absolute = function(a) {
    if(a.isNegative()) {
        return $hs.fastNegate(a);
    }
    return a;
};
$hs.googToGMP = function(a) {
    var bits = $hs.absolute(a).bits_;
    var s = bits.length;
    while(s !== 0 && bits[s-1] === 0)
        s--;
    return [a.isNegative() ? -s : s, a];
};
$hs.gmpToGoog = function(s,bits) {
    // If s is 0 then the number we want is 0
    if(s===0) {
        return goog.math.Integer.ZERO;
    }

    // We are going to need to know long the bits array should be
    var len = s < 0 ? -s : s;

    // Try to avoid making a new integer if we can
    if(bits instanceof goog.math.Integer) {
        var current = $hs.googToGMP(bits);
        // Make sure we do not have too many bits
        if(len >= current[0]) {
            if((s<0) === bits.isNegative()) {
                // Both same sign so just return bits
                return bits;
            }
            else {
                // Only the sign is changed
                return $hs.fastNegate(bits);
            }
        }
    }

    // Ok lets build a new one
    var newBits = [];
    for(var n = 0; n !== len; n++) {
        newBits[n] = $hs.indexIntArrayzh(bits, n);
    }
    var i = new goog.math.Integer(newBits, 0);
    return s < 0 ? $hs.fastNegate(i) : i;
};
$hs.indexIntArrayzh = function (a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        if(a instanceof goog.math.Integer) {
            return $hs.absolute(a).getBits(n);
        }
        return new Int32Array(a[0])[n];
    }
    else {
        var n2 = n<<1;
        if(a instanceof goog.math.Integer) {
            var positive = $hs.absolute(a);
            return goog.math.Long.fromBits(positive.getBits(n2), positive.getBits(n2+1));
        }
        var x = new Int32Array(a[0]);
        return goog.math.Long.fromBits(x[n2], x[n2+1]);
    }
};
$hs.indexWordArrayzh = function (a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Uint32Array(a[0])[n];
    }
    else {
        var x = new Int32Array(a[0], n<<3);
        return goog.math.Long.fromBits(x[0], x[1]);
    }
};
$hs.indexAddrArrayzh = function (a, n) {
    var res = a[0].ptrs[n];
    if(HS_DEBUG && res[2] !== new Uint32Array(a[0])[n])
        throw "Array Pointer Error";
    return res;
};
$hs.indexFloatArrayzh = function (a, n) {
    return Float32Array(a[0])[n];
};
$hs.indexDoubleArrayzh = function (a, n) {
    return Float64Array(a[0])[n];
};
$hs.indexStablePtrArrayzh = function (a, n) {
    var res = a[0].ptrs[n];
    if(HS_DEBUG && res[2] !== new Uint32Array(a[0])[n])
        throw "Array Pointer Error";
    return res;
};
$hs.indexInt8Arrayzh = function (a, n) {
    return new Int8Array(a[0])[n];
};
$hs.indexInt16Arrayzh = function (a, n) {
    return new Int16Array(a[0])[n];
};
$hs.indexInt32Arrayzh = function (a, n) {
    return new Int32Array(a[0])[n];
};
$hs.indexInt64Arrayzh = function (a, n) {
    var x = new Int32Array(a[0], n<<3);
    return goog.math.Long.fromBits(x[0], x[1]);
};
$hs.indexWord8Arrayzh = function (a, n) {
    return new Uint8Array(a[0])[n];
};
$hs.indexWord16Arrayzh = function (a, n) {
    return new Uint16Array(a[0])[n];
};
$hs.indexWord32Arrayzh = function (a, n) {
    return new Uint32Array(a[0])[n];
};
$hs.indexWord64Arrayzh = function (a, n) {
    var x = new Int32Array(a[0], n<<3);
    return goog.math.Long.fromBits(x[0], x[1]);
};
$hs.readCharArrayzh = function (a, n, s) {
    return [s, String.fromCharCode(new Uint8Array(a[0])[n])];
};
$hs.readWideCharArrayzh = function (a, n, s) {
    return [s, String.fromCharCode(new Uint32Array(a[0])[n])];
};
$hs.readIntArrayzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Int32Array(a[0])[n]];
    }
    else {
        var x = new Int32Array(a[0], n<<3);
        return [s, goog.math.Long.fromBits(x[0], x[1])];
    }
};
$hs.readWordArrayzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Uint32Array(a[0])[n]];
    }
    else {
        var x = new Int32Array(a[0], n<<3);
        return [s, goog.math.Long.fromBits(x[0], x[1])];
    }
};
$hs.readAddrArrayzh = function (a, n, s) {
    var res = a[0].ptrs[n];
    if(HS_DEBUG && res[2] !== new Uint32Array(a[0])[n])
        throw "Array Pointer Error";
    return [s, res];
};
$hs.readFloatArrayzh = function (a, n, s) {
    return [s, Float32Array(a[0])[n]];
};
$hs.readDoubleArrayzh = function (a, n, s) {
    return [s, Float64Array(a[0])[n]];
};
$hs.readStablePtrArrayzh = function (a, n, s) {
    var res = a[0].ptrs[n];
    if(HS_DEBUG && res[2] !== new Uint32Array(a[0])[n])
        throw "Array Pointer Error";
    return [s, res];
};
$hs.readInt8Arrayzh = function (a, n, s) {
    return [s, new Int8Array(a[0])[n]];
};
$hs.readInt16Arrayzh = function (a, n, s) {
    return [s, new Int16Array(a[0])[n]];
};
$hs.readInt32Arrayzh = function (a, n, s) {
    return [s, new Int32Array(a[0])[n]];
};
$hs.readInt64Arrayzh = function (a, n, s) {
    var x = new Int32Array(a[0], n<<3);
    return [s, goog.math.Long.fromBits(x[0], x[1])];
};
$hs.readWord8Arrayzh = function (a, n, s) {
    return [s, new Uint8Array(a[0])[n]];
};
$hs.readWord16Arrayzh = function (a, n, s) {
    return [s, new Uint16Array(a[0])[n]];
};
$hs.readWord32Arrayzh = function (a, n, s) {
    return [s, new Uint32Array(a[0])[n]];
};
$hs.readWord64Arrayzh = function (a, n, s) {
    var x = new Int32Array(a[0], n<<3);
    return [s, goog.math.Long.fromBits(x[0], x[1])];
};
$hs.writeCharArrayzh = function (a, n, v, s) {
    new Uint8Array(a[0])[n] = v.charCodeAt();
    return s;
};
$hs.writeWideCharArrayzh = function (a, n, v, s) {
    new Uint32Array(a[0])[n] = v.charCodeAt();
    return s;
};
$hs.writeIntArrayzh = function (a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Int32Array(a[0])[n] = v;
        return s;
    }
    else {
        var x = new Int32Array(a[0], n<<3);
        x[0] = v.getLowBits();
        x[1] = v.getHighBits();
        return s;
    }
};
$hs.writeWordArrayzh = function (a, n, v, s) {
    if(WORD_SIZE_IN_BITS==32) {
        new Uint32Array(a[0])[n] = v;
        return s;
    }
    else {
        var x = new Int32Array(a[0], n<<3);
        x[0] = v.getLowBits();
        x[1] = v.getHighBits();
        return s;
    }
};
$hs.writeAddrArrayzh = function (a, n, v, s) {
    a[0].ptrs[n] = v;
    new Uint32Array(a[0])[n] = v[2];
    return s;
};
$hs.writeFloatArrayzh = function (a, n, v, s) {
    Float32Array(a[0])[n] = v;
    return s;
};
$hs.writeDoubleArrayzh = function (a, n, v, s) {
    Float64Array(a[0])[n] = v;
    return s;
};
$hs.writeStablePtrArrayzh = function (a, n, v, s) {
    a[0].ptrs[n] = v;
    new Uint32Array(a[0])[n] = v[2];
    return s;
};
$hs.writeInt8Arrayzh = function (a, n, v, s) {
    new Int8Array(a[0])[n] = v;
    return s;
};
$hs.writeInt16Arrayzh = function (a, n, v, s) {
    new Int16Array(a[0])[n] = v;
    return s;
};
$hs.writeInt32Arrayzh = function (a, n, v, s) {
    new Int32Array(a[0])[n] = v;
    return s;
};
$hs.writeInt64Arrayzh = function (a, n, v, s) {
    var x = new Int32Array(a[0], n<<3);
    x[0] = v.getLowBits();
    x[1] = v.getHighBits();
    return s;
};
$hs.writeWord8Arrayzh = function (a, n, v, s) {
    new Uint8Array(a[0])[n] = v;
    return s;
};
$hs.writeWord16Arrayzh = function (a, n, v, s) {
    new Uint16Array(a[0])[n] = v;
    return s;
};
$hs.writeWord32Arrayzh = function (a, n, v, s) {
    new Uint32Array(a[0])[n] = v;
    return s;
};
$hs.writeWord64Arrayzh = function (a, n, v, s) {
    var x = new Int32Array(a[0], n<<3);
    x[0] = v.getLowBits();
    x[1] = v.getHighBits();
    return s;
};
$hs.copyByteArrayzh = function (src, soff, dest, doff, count, s) {
    srcarray = new Uint8Array(src[0]);
    destarray = new Uint8Array(dest[0]);
    while(count != 0) {
        destarray[doff] = srcarray[soff];
        soff++;
        doff++;
        count--;
    }
    return s;
};
$hs.copyMutableByteArrayzh = function (src, soff, dest, doff, count, s) {
    srcarray = new Uint8Array(src[0]);
    destarray = new Uint8Array(dest[0]);
    while(count != 0) {
        destarray[doff] = srcarray[soff];
        soff++;
        doff++;
        count--;
    }
    return s;
};
$hs.plusAddrzh = function (a, n) {
    if(typeof(a) === 'string')
        return [a, n, n];
    else
        return [a[0],a[1]+n,a[2]+n];
};
$hs.minusAddrzh = function (a, b) {
    return a[1]-b[1];
};
$hs.remAddrzh = function (a, b) {
    return a[1]%b;
};
$hs.gtAddrzh = function (a, b) {
    return $hs.mkBool(
         (a===null&&b===null)?false:
        ((a===null&&b!==null)?false:
        ((a!==null&&b===null)?true:(a[2]>b[2]))));
};
$hs.geAddrzh = function (a, b) {
    return $hs.mkBool(
         (a===null&&b===null)?true:
        ((a===null&&b!==null)?false:
        ((a!==null&&b===null)?true:(a[2]>=b[2]))));
};
$hs.eqAddrzh = function (a, b) {
    return $hs.mkBool(
         (a===null&&b===null)?true:
        ((a===null&&b!==null)?false:
        ((a!==null&&b===null)?false:(a[2]===b[2]))));
};
$hs.neAddrzh = function (a, b) {
    return $hs.mkBool(
         (a===null&&b===null)?false:
        ((a===null&&b!==null)?true:
        ((a!==null&&b===null)?true:(a[2]!==b[2]))));
};
$hs.ltAddrzh = function (a, b) {
    return $hs.mkBool(
         (a===null&&b===null)?false:
        ((a===null&&b!==null)?true:
        ((a!==null&&b===null)?false:(a[2]<b[2]))));
};
$hs.leAddrzh = function (a, b) {
    return $hs.mkBool(
         (a===null&&b===null)?true:
        ((a===null&&b!==null)?true:
        ((a!==null&&b===null)?false:(a[2]<=b[2]))));
};
$hs.indexCharOffAddrzh = function (a, n) {
    if(typeof(a) === 'string')
        return a.charAt(n);
    else if(typeof(a[0]) === 'string')
        return a[0].charAt(a[1]+n);
    else
        return String.fromCharCode(new Uint8Array(a[0],a[1]+n)[0]);
};
$hs.indexWideCharOffAddrzh = function (a, n) {
    return String.fromCharCode(new Uint32Array(a[0],a[1]+(n<<2))[0]);
};
$hs.indexIntOffAddrzh = function (a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Int32Array(a[0],a[1]+(n<<2))[0];
    }
    else {
        var x = new Int32Array(a[0], a[1] + (n<<3));
        return goog.math.Long.fromBits(x[0], x[1]);
    }
};
$hs.indexWordOffAddrzh = function (a, n) {
    if(WORD_SIZE_IN_BITS==32) {
        return new Uint32Array(a[0],a[1]+(n<<2))[0];
    }
    else {
        var x = new Int32Array(a[0], a[1] + (n<<3));
        return goog.math.Long.fromBits(x[0], x[1]);
    }
};
$hs.indexAddrOffAddrzh = function (a, n) {
    var res = a[0].ptrs[a[1]+(n<<2)];
    if(HS_DEBUG && res[2] !== new Uint32Array(a[0],a[1]+(n<<2))[0])
        throw "Array Pointer Error";
    return res;
};
$hs.indexFloatOffAddrzh = function (a, n) {
    return Float32Array(a[0],a[1]+(n<<2))[0];
};
$hs.indexDoubleOffAddrzh = function (a, n) {
    return Float64Array(a[0],a[1]+(n<<3))[0];
};
$hs.indexStablePtrOffAddrzh = function (a, n) {
    var res = a[0].ptrs[a[1]+(n<<2)];
    if(HS_DEBUG && res[2] !== new Uint32Array(a[0],a[1]+(n<<2))[0])
        throw "Array Pointer Error";
    return res;
};
$hs.indexInt8OffAddrzh = function (a, n) {
    return new Int8Array(a[0],a[1]+n)[0];
};
$hs.indexInt16OffAddrzh = function (a, n) {
    return new Int16Array(a[0],a[1]+(n<<1))[0];
};
$hs.indexInt32OffAddrzh = function (a, n) {
    return new Int32Array(a[0],a[1]+(n<<2))[0];
};
$hs.indexInt64OffAddrzh = function (a, n) {
    var x = new Uint32Array(a[0],a[1]+(n<<3));
    return goog.math.Long.fromBits(x[0], x[1]);
};
$hs.indexWord8OffAddrzh = function (a, n) {
    if(typeof(a) === 'string')
        return a.charCodeAt(n);
    else if(typeof(a[0]) === 'string')
        return a[0].charCodeAt(a[1]+n);
    else
        return new Uint8Array(a[0],a[1]+n)[0];
};
$hs.indexWord16OffAddrzh = function (a, n) {
    return new Uint16Array(a[0],a[1]+(n<<1))[0];
};
$hs.indexWord32OffAddrzh = function (a, n) {
    return new Uint32Array(a[0],a[1]+(n<<2))[0];
};
$hs.indexWord64OffAddrzh = function (a, n) {
    var x = new Uint32Array(a[0],a[1]+(n<<3));
    return goog.math.Long.fromBits(x[0], x[1]);
};
$hs.readCharOffAddrzh = function (a, n, s) {
    return [s, String.fromCharCode(new Uint8Array(a[0],a[1]+n)[0])];
};
$hs.readWideCharOffAddrzh = function (a, n, s) {
    return [s, String.fromCharCode(new Uint32Array(a[0],a[1]+(n<<2))[0])];
};
$hs.readIntOffAddrzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Int32Array(a[0],a[1]+(n<<2))[0]];
    }
    else {
        var x = new Int32Array(a[0], a[1] + (n<<3));
        return [s, goog.math.Long.fromBits(x[0], x[1])];
    }
};
$hs.readWordOffAddrzh = function (a, n, s) {
    if(WORD_SIZE_IN_BITS==32) {
        return [s, new Uint32Array(a[0],a[1]+(n<<2))[0]];
    }
    else {
        var x = new Int32Array(a[0], a[1] + (n<<3));
        return [s, goog.math.Long.fromBits(x[0], x[1])];
    }
};
$hs.readAddrOffAddrzh = function (a, n, s) {
    var res = a[0].ptrs[a[1]+(n<<2)];
    if(HS_DEBUG && a[2] !== new Uint32Array(a[0],a[1]+(n<<2))[0])
        throw "Array Pointer Error";
    return [s, res];
};
$hs.readFloatOffAddrzh = function (a, n, s) {
    return [s, Float32Array(a[0],a[1]+(n<<2))[0]];
};
$hs.readDoubleOffAddrzh = function (a, n, s) {
    return [s, Float64Array(a[0],a[1]+(n<<3))[0]];
};
$hs.readStablePtrOffAddrzh = function (a, n, s) {
    var res = a[0].ptrs[a[1]+(n<<2)];
    if(HS_DEBUG && a[2] !== new Uint32Array(a[0],a[1]+(n<<2))[0])
        throw "Array Pointer Error";
    return [s, res];
};
$hs.readInt8OffAddrzh = function (a, n, s) {
    return [s, new Int8Array(a[0],a[1]+n)[0]];
};
$hs.readInt16OffAddrzh = function (a, n, s) {
    return [s, new Int16Array(a[0],a[1]+(n<<1))[0]];
};
$hs.readInt32OffAddrzh = function (a, n, s) {
    return [s, new Int32Array(a[0],a[1]+(n<<2))[0]];
};
$hs.readInt64OffAddrzh = function (a, n, s) {
    var x = new Uint32Array(a[0],a[1]+(n<<3));
    return [s, goog.math.Long.fromBits(x[0], x[1])];
};
$hs.readWord8OffAddrzh = function (a, n, s) {
    if(typeof(a) === 'string')
        return [s, a.charCodeAt(n)];
    else
        return [s, new Uint8Array(a[0],a[1]+n)[0]];
};
$hs.readWord16OffAddrzh = function (a, n, s) {
    return [s, new Uint16Array(a[0],a[1]+(n<<1))[0]];
};
$hs.readWord32OffAddrzh = function (a, n, s) {
    return [s, new Uint32Array(a[0],a[1]+(n<<2))[0]];
};
$hs.readWord64OffAddrzh = function (a, n, s) {
    var x = new Uint32Array(a[0],a[1]+(n<<3));
    return [s, goog.math.Long.fromBits(x[0], x[1])];
};
$hs.writeCharOffAddrzh = function (a, n, v, s) {
    (new Uint8Array(a[0],a[1]+n))[0] = v.charCodeAt();
    return s;
};
$hs.writeWideCharOffAddrzh = function (a, n, v, s) {
    (new Uint32Array(a[0],a[1]+(n<<2)))[0] = v.charCodeAt();
    return s;
};
$hs.writeIntOffAddrzh = function (a, n, v, s) {
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
$hs.writeWordOffAddrzh = function (a, n, v, s) {
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
$hs.writeAddrOffAddrzh = function (a, n, v, s) {
    a[0].ptrs[n] = v;
    (new Uint32Array(a[0],a[1]+(n<<2)))[0] = a[2];
    return s;
};
$hs.writeFloatOffAddrzh = function (a, n, v, s) {
    (new Float32Array(a[0],a[1]+(n<<2)))[0] = v;
    return s;
};
$hs.writeDoubleOffAddrzh = function (a, n, v, s) {
    (new Float64Array(a[0],a[1]+(n<<3)))[0] = v;
    return s;
};
$hs.writeStablePtrOffAddrzh = function (a, n, v, s) {
    a[0].ptrs[n] = v;
    (new Uint32Array(a[0],a[1]+(n<<2)))[0] = a[2];
    return s;
};
$hs.writeInt8OffAddrzh = function (a, n, v, s) {
    (new Int8Array(a[0],a[1]+n))[0] = v;
    return s;
};
$hs.writeInt16OffAddrzh = function (a, n, v, s) {
    (new Int16Array(a[0],a[1]+(n<<1)))[0] = v;
    return s;
};
$hs.writeInt32OffAddrzh = function (a, n, v, s) {
    (new Int32Array(a[0],a[1]+(n<<2)))[0] = v;
    return s;
};
$hs.writeInt64OffAddrzh = function (a, n, v, s) {
    var x = new Int32Array(a[0], a[1]+(n<<3));
    x[0] = v.getLowBits();
    x[1] = v.getHighBits();
    return s;
};
$hs.writeWord8OffAddrzh = function (a, n, v, s) {
    (new Uint8Array(a[0],a[1]+n))[0] = v;
    return s;
};
$hs.writeWord16OffAddrzh = function (a, n, v, s) {
    (new Uint16Array(a[0],a[1]+(n<<1)))[0] = v;
    return s;
};
$hs.writeWord32OffAddrzh = function (a, n, v, s) {
    (new Uint32Array(a[0],a[1]+(n<<2)))[0] = v;
    return s;
};
$hs.writeWord64OffAddrzh = function (a, n, v, s) {
    var x = new Int32Array(a[0], a[1]+(n<<3));
    x[0] = v.getLowBits();
    x[1] = v.getHighBits();
    return s;
};
$hs.alert = function (str) {
    window.alert(str);
};
$hs.logAny = function (c, str) {
    var el = document.getElementById('log');
    el.innerHTML = el.innerHTML + c + ": " + str + '<br/>\n';
};
$hs.logInfo = function (str) {
    $hs.logAny("INFO", str);
};
$hs.logError = function (str) {
    $hs.logAny("ERROR", str);
};
$hs.logDebug = function (str) {
    $hs.logAny("DEBUG", str);
};

$hs.logger = goog.debug.Logger.getLogger('hs');

$hs.utf32 = function(s) {
    var res = $hs.newByteArrayzh((s.length<<2)+4)[1];
    var dest = new Uint32Array(res[0]);
    for(var i=0;i!=s.length;i++)
        dest[i]=s.charCodeAt(i);
    dest[i]=0;
    return res;
};
$hs.fromUtf32 = function(s) {
    var res = "";
    var src = new Uint32Array(s[0],s[1]);
    var len = src[src.length-1] === 0 ? src.length - 1 : src.length;
    for(var i=0;i!=len;i++)
        res=res+String.fromCharCode(src[i]);
    return res;
};
$hs.ascii = function(s) {
    var res = $hs.newByteArrayzh(s.length+1)[1];
    var dest = new Uint8Array(res[0]);
    for(var i=0;i!=s.length;i++)
        dest[i]=s.charCodeAt(i);
    dest[i]=0;
    return res;
};
var integer_cmm_cmpIntegerzh = function(sa, abits, sb, bbits) {
    return $hs.gmpToGoog(sa, abits).compare($hs.gmpToGoog(sb, bbits));
};
var integer_cmm_cmpIntegerIntzh = function(sa, abits, b) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs.gmpToGoog(sa, abits).compare(goog.math.Integer.fromInt(b));
    }
    else {
        return $hs.gmpToGoog(sa, abits).compare(
            goog.math.Integer.fromBits([b.getLowBits(), b.getHighBits()]));
    }
};
var integer_cmm_plusIntegerzh = function(sa, abits, sb, bbits) {
    return $hs.googToGMP($hs.gmpToGoog(sa, abits).add($hs.gmpToGoog(sb, bbits)));
};
var integer_cmm_minusIntegerzh = function(sa, abits, sb, bbits) {
    return $hs.googToGMP($hs.gmpToGoog(sa, abits).subtract($hs.gmpToGoog(sb, bbits)));
};
var integer_cmm_timesIntegerzh = function(sa, abits, sb, bbits) {
    return $hs.googToGMP($hs.gmpToGoog(sa, abits).multiply($hs.gmpToGoog(sb, bbits)));
};
var integer_cmm_quotRemIntegerzh = function(sa, abits, sb, bbits) {
    var a = $hs.gmpToGoog(sa, abits);
    var b = $hs.gmpToGoog(sb, bbits)
    var q = a.divide(b);
    var r = a.subtract(q.multiply(b));
    return $hs.googToGMP(q).concat($hs.googToGMP(r));
};
var integer_cmm_quotIntegerzh = function(sa, abits, sb, bbits) {
    return $hs.googToGMP($hs.gmpToGoog(sa, abits).divide($hs.gmpToGoog(sb, bbits)));
};
var integer_cmm_remIntegerzh = function(sa, abits, sb, bbits) {
    return $hs.googToGMP($hs.gmpToGoog(sa, abits).modulo($hs.gmpToGoog(sb, bbits)));
};
var integer_cmm_divModIntegerzh = function(sa, abits, sb, bbits) {
    var a = $hs.gmpToGoog(sa, abits);
    var b = $hs.gmpToGoog(sb, bbits);
    var d = a.divide(b);
    var m = a.subtract(d.multiply(b));
    if(a.isNegative()!==b.isNegative() && !m.isZero()) {
        // Take one off d and add b onto m
        d = d.add(goog.math.Integer.fromInt(-1));
        m = m.add(b);
    }
    return $hs.googToGMP(d).concat($hs.googToGMP(m));
};
var integer_cmm_divExactIntegerzh = function(sa, abits, sb, bbits) {
    return $hs.googToGMP($hs.gmpToGoog(sa, abits).divide($hs.gmpToGoog(sb, bbits)));
};
$hs.gcd = function(a, b) {
    var x = $hs.absolute(a);
    var y = $hs.absolute(b);
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
    return $hs.googToGMP(big);
};
var integer_cmm_gcdIntegerzh = function(sa, abits, sb, bbits) {
    return $hs.gcd($hs.gmpToGoog(sa, abits), $hs.gmpToGoog(sb, bbits));
};
var integer_cmm_gcdIntegerIntzh = function(sa, abits, b) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs.gcd($hs.gmpToGoog(sa, abits), goog.math.Integer.fromInt(b));
    }
    else {
        return $hs.gcd($hs.gmpToGoog(sa, abits),
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
    // 60bits is more than the 53 that the flote has and small enought to fit
    // in just 2 32 bit ints.
    var exponent = 60 - (Math.log(x)/0.6931471805599453); // Math.log(2)
    return [exponent].concat($hs.googToGMP(
        goog.math.Integer.fromNumber(x * Math.pow(2, exponent))));
};
var integer_cmm_int2Integerzh = function(i) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs.googToGMP(goog.math.Integer.fromInt(i));
    }
    else {
        return $hs.googToGMP(
            goog.math.Integer.fromBits([i.getLowBits(), i.getHighBits()]));
    }
};
var integer_cmm_word2Integerzh = function(i) {
    if(WORD_SIZE_IN_BITS==32) {
        return $hs.googToGMP(goog.math.Integer.fromBits(i<0?[0,i]:[i]));
    }
    else {
        return $hs.googToGMP(goog.math.Integer.fromBits(
            i.isNegative()?[0, i.getLowBits(), i.getHighBits()]
                          :[   i.getLowBits(), i.getHighBits()]));
    }
};
var integer_cmm_andIntegerzh = function(sa, abits, sb, bbits) {
    return $hs.googToGMP($hs.gmpToGoog(sa, abits).and($hs.gmpToGoog(sb, bbits)));
};
var integer_cmm_orIntegerzh = function(sa, abits, sb, bbits) {
    return $hs.googToGMP($hs.gmpToGoog(sa, abits).or($hs.gmpToGoog(sb, bbits)));
};
var integer_cmm_xorIntegerzh = function(sa, abits, sb, bbits) {
    return $hs.googToGMP($hs.gmpToGoog(sa, abits).xor($hs.gmpToGoog(sb, bbits)));
};
var integer_cmm_mul2ExpIntegerzh = function(sa, abits, b) {
    return $hs.googToGMP($hs.gmpToGoog(sa, abits).shiftLeft(b));
};
var integer_cmm_fdivQ2ExpIntegerzh = function(sa, abits, b) {
    return $hs.googToGMP($hs.gmpToGoog(sa, abits).shiftRight(b));
};
var integer_cmm_complementIntegerzh = function(sa, abits) {
    return $hs.googToGMP($hs.gmpToGoog(sa, abits).not());
};
var integer_cmm_int64ToIntegerzh = function(a) {
    return $hs.googToGMP(goog.math.Integer.fromBits([a.getLowBits(), a.getHighBits()]));
};
var integer_cmm_word64ToIntegerzh = function(a) {
    return $hs.googToGMP(goog.math.Integer.fromBits(
            a.isNegative()?[0, a.getLowBits(), a.getHighBits()]
                          :[   a.getLowBits(), a.getHighBits()]));
};
var hs_integerToInt64 = function(as, abits) {
    var a = $hs.gmpToGoog(as, abits);
    return goog.math.Long.fromBits(a.getBits(0), a.getBits(1));
};
var hs_integerToWord64 = function(as, abits) {
    var a = $hs.gmpToGoog(as, abits);
    return goog.math.Long.fromBits(a.getBits(0), a.getBits(1));
};

var _hs_text_memcpy = function (dest, doff, src, soff, count) {
    srcarray = new Uint16Array(src[0],src[1]);
    destarray = new Uint16Array(dest[0],dest[1]);
    while(count != 0) {
        destarray[doff] = srcarray[soff];
        soff++;
        doff++;
        count--;
    }
    return dest;
};

var _hs_text_memcmp = function(a, aoff, b, boff, count) {
    aarray = new Uint16Array(a[0],a[1]);
    barray = new Uint16Array(b[0],b[1]);
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
var memcpy = function(dest, src, count) {
    if(typeof(src) === 'string') {
        destarray = new Uint8Array(dest[0],dest[1]);
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
        destarray = new Uint8Array(dest[0],dest[1]);
        srcarray = new Uint8Array(src[0],src[1]);
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

var u_iswalpha = function(a) {
    return goog.string.isAlpha(String.fromCharCode(a)) ? 1 : 0;
};
var u_iswalnum = function(a) {
    return goog.string.isAlphaNumeric(String.fromCharCode(a)) ? 1 : 0;
};
var u_iswspace = function(a) {
    return goog.string.isSpace(String.fromCharCode(a)) ? 1 : 0;
};
var u_iswlower = function(a) {
    return a === u_towupper(a) ? 1 : 0;
};
var u_iswupper = function(a) {
    return a === u_towlower(a) ? 1 : 0;
};
var u_towlower = function(a) { return String.fromCharCode(a).toLowerCase().charCodeAt(); };
var u_towupper = function(a) { return String.fromCharCode(a).toUpperCase().charCodeAt(); };
var rtsSupportsBoundThreads = function () { return 0; };
var getOrSetGHCConcSignalSignalHandlerStore = function (x) { return x; };
var stg_sig_install = function(a,b,c) { return -1; };
var localeEncoding = function() { return $hs.ascii("UTF-32LE"); };
var hs_iconv_open = function(to,from) { return 1; };
var hs_iconv_close = function(h) { return 0; };

$hs.unsignedCompare = function(a,b) {
    if (a.equals(b)) return 0;

    var aneg = a.isNegative();
    var bneg = b.isNegative();
    if (aneg && !bneg) return 1;
    if (!aneg && bneg) return -1;

    return a.subtract(b).isNegative() ? -1 : 1;
};

if(WORD_SIZE_IN_BITS == 64) {
    // Int primatives for 64bit
    $hs.quotIntzh = function(a, b) {
       return a.div(b); };
    $hs.remIntzh = function(a, b) {
       return a.modulo(b); };
    $hs.int2Wordzh = function(a) {
       return a; };
    $hs.int2Floatzh = function(a) {
       return a.toNumber(); };
    $hs.int2Doublezh = function(a) {
       return a.toNumber(); };
    $hs.uncheckedIShiftLzh = function(a, b) {
       return a.shiftLeft(b.toNumber()); };
    $hs.uncheckedIShiftRAzh = function(a, b) {
       return a.shiftRight(b.toNumber()); };
    $hs.uncheckedIShiftRLzh = function(a, b) {
       return a.shiftRight(b.toNumber()); };

    // Word primatives for 64bit
    $hs.quotWordzh = function(a, b) {
       return a.div(b); };   // TODO make unsigned
    $hs.remWordzh = function(a, b) {
       return a.modulo(b); }; // TODO make unsigned
    $hs.uncheckedShiftLzh = function(a, b) {
       return a.shiftLeft(b.toNumber()); };
    $hs.uncheckedShiftRLzh = function(a, b) {
       return a.shiftRight(b.toNumber()); };
    $hs.word2Intzh = function(a) {
       return a; };
    $hs.gtWordzh = function(a, b) {
       return $hs.mkBool($hs.unsignedCompare(a, b) > 0); };
    $hs.geWordzh = function(a, b) {
       return $hs.mkBool($hs.unsignedCompare(a, b) >= 0); };
    $hs.eqWordzh = function(a, b) {
       return $hs.mkBool(a.equals(b)); };
    $hs.neWordzh = function(a, b) {
       return $hs.mkBool(a.notEquals(b)); };
    $hs.ltWordzh = function(a, b) {
       return $hs.mkBool($hs.unsignedCompare(a, b) < 0); };
    $hs.leWordzh = function(a, b) {
       return $hs.mkBool($hs.unsignedCompare(a, b) <= 0); };

    $hs.intToIntzh = function(i) {
       return goog.math.Long.fromInt(i); };
    $hs.intzhToInt = function(i) {
       return i.toInt(); };
    $hs.int64ToWordzh = function(i) {
       return i; };
    $hs.wordToWordzh = function(i) {
       return goog.math.Long.fromBits(i,0); };
    $hs.wordzhToWord = function(i) {
       return i.getLowBits(); };
    $hs.word64ToIntzh = function(i) {
       return i; };
}

var hs_gtWord64 = function(a, b) {
   return $hs.unsignedCompare(a, b) > 0 ? 1 : 0; };
var hs_geWord64 = function(a, b) {
   return $hs.unsignedCompare(a, b) >= 0 ? 1 : 0; };
var hs_eqWord64 = function(a, b) {
   return a.equals(b) ? 1 : 0; };
var hs_neWord64 = function(a, b) {
   return a.notEquals(b) ? 1 : 0; };
var hs_ltWord64 = function(a, b) {
   return $hs.unsignedCompare(a, b) < 0 ? 1 : 0; };
var hs_leWord64 = function(a, b) {
   return $hs.unsignedCompare(a, b) <= 0 ? 1 : 0; };

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
    HS_RTS_TRACE && $hs.logger.info('__hscore_get_errno');
    return errno;
}
var __hscore_set_errno = function(e) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_set_errno');
    errno = e;
};
var strerror = function(e) {
    return $hs.utf32("Error "+e);
}
var __hscore_s_isreg = function(m) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_s_isreg');
    return 1;
};
var __hscore_s_isdir = function(m) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_s_isdir');
    return 0;
};
var __hscore_s_isfifo = function(m) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_s_isfifo');
    return 0;
};
var __hscore_s_isblk = function(m) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_s_isblk');
    return 0;
};
var __hscore_s_ischr = function(m) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_s_ischr');
    return 0;
};
var __hscore_s_issock = function(m) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_s_issock');
    return 0;
};

var __hscore_sigemptyset = function(set) { return 0; };
var __hscore_sigfillset = function(set) { return 0; };
var __hscore_sigaddset = function(set, s) { return 0; };
var __hscore_sigdelset = function(set, s) { return 0; };
var __hscore_sigismember = function(set, s) { return 0; };

var __hscore_memcpy_src_off = function(dest, src, soff, count) {
    var doff = 0;
    srcarray = new Uint8Array(src[0]);
    destarray = new Uint8Array(dest[0]);
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
    HS_RTS_TRACE && $hs.logger.info('__hscore_ftruncate');
    return 0;
};
var __hscore_setmode = function(fd, toBin) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_setmode');
    return 0;
};

var __hscore_sizeof_stat = function() { return 4; };
var __hscore_st_mtime = function(st) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_st_mtime');
    return 0;
};
var __hscore_st_size = function(st) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_st_size');
    return 0;
};
var __hscore_st_mode = function(st) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_st_mode');
    return 0;
};
var __hscore_st_dev = function(st) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_st_dev');
    return 0;
};
var __hscore_st_ino = function(st) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_st_ino');
    return 0;
};
var __hscore_stat = function(f, buf) {
    var p = $hs.fromUtf32(f);
    HS_RTS_TRACE && $hs.logger.info('__hscore_stat '+p);
    return 0;
};
var __hscore_fstat = function(fd, buf) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_fstat');
    return 0;
};
var __hscore_lstat = function(f, buf) {
    var p = $hs.fromUtf32(f);
    HS_RTS_TRACE && $hs.logger.info('__hscore_lstat '+p);
    return 0;
};

var __hscore_lflag = function(ts) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_fstat');
    return ts.c_lflag;
};
var __hscore_poke_lflag = function(ts, t) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_poke_lflag');
    ts.c_lflag = t;
};
var __hscore_ptr_c_cc = function(ts) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_ptr_c_cc');
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
    HS_RTS_TRACE && $hs.logger.info('__hscore_get_saved_termios');
    return null;
};
var __hscore_set_saved_termios = function(fd, ts) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_set_saved_termios');
};
var __hscore_hs_fileno = function(f) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_hs_fileno');
    return 0;
};
$hs.fstab = [];
$hs.getFileURL = function(f) {
    for(var i=0;i!==$hs.fstab.length;i++) {
        if(f.slice(0, $hs.fstab[i].mountPoint.length) === $hs.fstab[i].mountPoint)
            return $hs.fstab[i].url + f.slice($hs.fstab[i].mountPoint.length);
    }
    return null;
};
$hs.allFiles = [
    { text:"", fptr:0 }, // stdin
    { text:"", fptr:0 }, // stdout
    { text:"", fptr:0 }  // stderr
];
var fdReady = function (fd) {
    if (fd >= $hs.allFiles.length) return -1;
    var f = $hs.allFiles[fd];
    return f.text.length > f.fptr ? 1 : 0;
};
$hs.findFile = function(f) {
    for(var i=0;i!==$hs.allFiles.length;i++) {
        if(f===$hs.allFiles[i].path)
            return i;
    }
    return -1;
};
var __hscore_open = function(f,h,m) {
    var p = $hs.fromUtf32(f);
    HS_RTS_TRACE && $hs.logger.info('__hscore_open '+p);
    var result=$hs.findFile(p);
    if(result===-1) {
        var url = $hs.getFileURL(p);
        if(url!==null) {
            try {
                var transport = new XMLHttpRequest();
                transport.open("GET", url, false);
                transport.send(null);
                if (transport.status == 200 || transport.status == 0) {
                    result = $hs.allFiles.length;
                    $hs.allFiles[result] = {text:transport.responseText, fptr:0, path:p};
                }
                else {
                   $hs.logError("Error " + transport.status + " opening file: " + p +" ( " + url + " )");
                }
            } catch (e) {
                $hs.logError("Error opening file: " + p + " ( " + url + " ) :\n" + e);
            }
        }
        else {
            if(m & __hscore_o_creat() !== 0) {
                result = $hs.allFiles.length;
                $hs.allFiles[result] = {text:"", fptr:0, path:p};
            }
        }
    }
    return result;
};
var close = function(fd) {
    HS_RTS_TRACE && $hs.logger.info('close');
    return 0;
};
var __hscore_lseek = function(fd, off, whence) {
    HS_RTS_TRACE && $hs.logger.info('__hscore_lseek');
    return 0;
};

var hsFD_SETSIZE = function() { return 1024; };
var hsFD_ISSET = function(fd, fds) {
    HS_RTS_TRACE && $hs.logger.info('hsFD_ISSET');
    return 0;
};
var hsFD_SET = function(fd, fds) {
    HS_RTS_TRACE && $hs.logger.info('hsFD_SET');
};
var sizeof_fd_set = function() {
    HS_RTS_TRACE && $hs.logger.info('sizeof_fd_set');
    return 0;
};
var hsFD_ZERO = function(fds) {
    HS_RTS_TRACE && $hs.logger.info('hsFD_ZERO');
};

var __hscore_select = function(nfds, readfds, writefds, exceptfds, timeout) {
    HS_RTS_TRACE && $hs.logger.info('hsFD_ZERO');
    return 0;
};

var environ = {};
environ['TMPDIR'] = $hs.utf32("/tmp");
var __hscore_environ = function() {
    HS_RTS_TRACE && $hs.logger.info('__hscore_environ');
    return environ;
};
var getenv = function(e) {
    var s = $hs.fromUtf32(e);
    HS_RTS_TRACE && $hs.logger.info('getenv '+s);
    var v = environ[s];
    return v===undefined?null:v;
};
var lockFile = function(fd, dev, ino, for_writing) {
    HS_RTS_TRACE && $hs.logger.info('lockFile');
    return 0;
};
var unlockFile = function(fd) {
    HS_RTS_TRACE && $hs.logger.info('unlockFile');
    return 0;
};
var isatty = function(fd) {
    HS_RTS_TRACE && $hs.logger.info('isatty');
    return 0;
};
var read = function(fd, p, s) {
    HS_RTS_TRACE && $hs.logger.info('read');
    var f = $hs.allFiles[fd];
    if (f === undefined || f === null)
        return -1;

    var n = f.text.length - f.fptr;
    if( n <= 0 ) {
        HS_RTS_TRACE && $hs.logger.info('read : end of file');
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

    HS_RTS_TRACE && $hs.logger.info('read : '+(n<<2));
    return n<<2;
};
var write = function(fd, p, s) {
    HS_RTS_TRACE && $hs.logger.info('write');
    var f = $hs.allFiles[fd];
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
    HS_RTS_TRACE && $hs.logger.info('initLinker');
};
var exitLinker = function() {
    HS_RTS_TRACE && $hs.logger.info('exitLinker');
};
$hs.loaded = [];
$hs.loadPath = "./";
$hs.load = function (modules) {
    for (var i = 0; i < modules.length; i++) {
        if($hs.loaded[modules[i]]===undefined) {
            var path = $hs.loadPath + "hs" + modules[i] + (COMPILED ? "min.js" : ".js");
            var transport = new XMLHttpRequest();
            transport.open("GET", path, false);
            transport.send(null);
            if (transport.status == 200 || transport.status == 0) {
                try {
                    $hs.loadingModule=true;
                    goog.globalEval(transport.responseText);
                    $hs.loadingModule=false;
                    $hs.loaded[modules[i]] = true;
                } catch (e) {
                    $hs.logError("Error evaluating module: " + path + ":\n" + e);
                    return false;
                }
            }
        }
    }
};

$hs.fromHaskellString = function(args, onComplete, onException) {
    console.log("fromHaskellString");
    var loop = function(res, a) {
        $hs.force(a, function(s) {
            switch (s.g) {
                case 1: // nil
                    onComplete(res);
                    break;
                case 2: // cons
                    var chthunk = s.v[0];
                    var sthunk = s.v[1];
                    $hs.force([chthunk], function(ch){
                        loop(res+ch.v[0],[sthunk]);}, onException);
                    break;
                default:
                    throw "undefined";
            }
        }, onException);
    }
    loop("", args);
};
$hs.fromHaskellInt = function(args, onComplete, onException) {
    console.log("fromHaskellInt");
    $hs.force(args, function(i){onComplete(i.v[0]);}, onException);
};
$hs.fromHaskellIO = function(args, onComplete, onException) {
    console.log("fromHaskellIO");
    var newArguments = [];
    for (var i = 0; i < args.length; i++)
        newArguments[i] = args[i];
    newArguments[args.length] = $$GHCziPrim_realWorldzh;
    $hs.force(newArguments, function(i){onComplete(i[1]);}, onException);
};
$hs.toHaskellInt = function(i) {
    console.log("toHaskellInt");
    return new $DataValue(1, [(0 + i) & ~0]);
};
var hs_nil = function() {
    return "";
};
var hs_cons = function(x, xs) {
    return String.fromCharCode(x) + xs;
};
$hs.init = function() {
    $$GHCziPrim_realWorldzh = $D(1, function(){return []}, "realWorld#");
    $$GHCziPrim_coercionTokenzh = $D(1, function(){return []}, "coercionToken#");
};
