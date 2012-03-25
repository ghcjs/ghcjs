/**
 * @define {boolean} HS_DEBUG is like goog.DEBUG, but for ghcjs internals
 */
var HS_DEBUG = true;

/**
 * @define {boolean} enable extra tracing
 */
var HS_TRACE = false;

var $hs = {
    // treat signed integer as unsigned word, assuming complementary representation
    Word : {
      gt : function(a, b) {
        if ((a < 0 || b < 0) && (a >= 0 || b >= 0)) // Different signs
          return a < b;
        else
          return a > b;
      },
      ge : function(a, b) {
        if ((a < 0 || b < 0) && (a >= 0 || b >= 0)) // Different signs
          return a <= b;
        else
          return a >= b;
      },
      lt : function(a, b) {
        if ((a < 0 || b < 0) && (a >= 0 || b >= 0)) // Different signs
          return a > b;
        else
          return a < b;
      },
      le : function(a, b) {
        if ((a < 0 || b < 0) && (a >= 0 || b >= 0)) // Different signs
          return a >= b;
        else
          return a <= b;
      }
    },
    Int : { // binary operations supposed to work with 32bit words
      addCarry : function(a, b, c) {
         var word16addCarry = function(a, b, c) {
            var sum = a + b + c;
            var res = sum & 0xFFFF;
            var carry = sum >>> 16;
            return [res, carry];
         };
         var resl = word16addCarry(a & 0xFFFF, b & 0xFFFF, c);
         var resh = word16addCarry(a >>> 16, b >>> 16, resl[1]);
         return [(resh[0] << 16) | resl[0], resh[1]];
      },
      mul : function(a, b) {
         var al = a & 0xFFFF;
         var ah = a >>> 16;
         var bl = b & 0xFFFF;
         var bh = b >>> 16;
         var r = $hs.Int.addCarry(al * bh, bl * ah, 0)[0]
         return $hs.Int.addCarry(al * bl, (r & 0xFFFF) << 16, 0)[0];
      },
    },
    modules: {},
    alert: function (str) {
        window.alert(str);
    },
    logAny: function (c, str) {
        var el = document.getElementById('log');
        el.innerHTML = el.innerHTML + c + ": " + str + '<br/>\n';
    },
    logInfo: function (str) {
        $hs.logAny("INFO", str);
    },
    logError: function (str) {
        $hs.logAny("ERROR", str);
    },
    logDebug: function (str) {
        $hs.logAny("DEBUG", str);
    },
    MutVar : {
        newMutVar : function(a, s) {
            return [s, {value : a}];
        },
        read : function (a, s) {
            return [s, a.value];
        },
        write : function (a, b, s) {
            a.value = b;
            return [s];
        },
        same : function (a, b) {
            return a === b;
        }
    },
    _Array : {
        newArray : function(n, a, s) {
            var result = [];
            for (x = 0; x != n; x++)
              result[x] = a;
            return [s, result];
        },
        same : function (a, b) {
            return a === b;
        },
        read : function (a, n, s) {
            return [s, a[n]];
        },
        write : function (a, n, b, s) {
            a[n] = b;
            return [s];
        },
        sizeof : function (a, s) {
            return [s, a.length];
        },
        sizeofMut : function (a, s) {
            return [s, a.length];
        },
        index : function (a, n) {
            return a[n];
        },
        unsafeFreeze : function (a, s) {
            return [s, a];
        },
        unsafeThaw : function (a, s) {
            return [s, a];
        }
    },
    ptrBase : 0,
    newByteArrayzh : function (n, s) {
        var result = [new ArrayBuffer(n), 0, $hs.ptrBase];
        result[0].ptrs=[];
        $hs.ptrBase += n;
        return [s, result];
    },
    newPinnedByteArrayzh : function (n, s) {
        var result = [new ArrayBuffer(n), 0, $hs.ptrBase];
        result[0].ptrs=[];
        $hs.ptrBase += n;
        return [s, result];
    },
    newAlignedPinnedByteArrayzh : function (n, k, s) {
        $hs.ptrBase += $hs.ptrBase%k;
        var result = [new ArrayBuffer(n), 0, $hs.ptrBase];
        result[0].ptrs=[];
        $hs.ptrBase += n;
        return [s, result];
    },
    byteArrayContentszh : function (a) {
        return a;
    },
    sameMutableByteArrayzh : function (a, b) {
        return a[2] === b[2];
    },
    unsafeFreezzeByteArrayzh : function (a, s) {
        return [s, a];
    },
    sizeofByteArrayzh : function (a) {
        return new Uint8Array(a[0]).length;
    },
    sizeofMutableByteArrayzh : function (a) {
        return new Uint8Array(a[0]).length;
    },
    indexCharArrayzh : function (a, n) {
        return String.fromCharCode(new Uint8Array(a[0])[n]);
    },
    indexWideCharArrayzh : function (a, n) {
        return String.fromCharCode(new Uint32Array(a[0])[n]);
    },
    indexIntArrayzh : function (a, n) {
        return new Int32Array(a[0])[n];
    },
    indexWordArrayzh : function (a, n) {
        return new Uint32Array(a[0])[n];
    },
    indexAddrArrayzh : function (a, n) {
        var res = a[0].ptrs[n];
        if(HS_DEBUG && res[2] !== new Uint32Array(a[0])[n])
            throw "Array Pointer Error";
        return res;
    },
    indexFloatArrayzh : function (a, n) {
        return Float32Array(a[0])[n];
    },
    indexDoubleArrayzh : function (a, n) {
        return Float64Array(a[0])[n];
    },
    indexStablePtrArrayzh : function (a, n) {
        var res = a[0].ptrs[n];
        if(HS_DEBUG && res[2] !== new Uint32Array(a[0])[n])
            throw "Array Pointer Error";
        return res;
    },
    indexInt8Arrayzh : function (a, n) {
        return new Int8Array(a[0])[n];
    },
    indexInt16Arrayzh : function (a, n) {
        return new Int16Array(a[0])[n];
    },
    indexInt32Arrayzh : function (a, n) {
        return new Int32Array(a[0])[n];
    },
    indexInt64Arrayzh : function (a, n) {
        return new Int64Array(a[0])[n];
    },
    indexWord8Arrayzh : function (a, n) {
        return new Uint8Array(a[0])[n];
    },
    indexWord16Arrayzh : function (a, n) {
        return new Uint16Array(a[0])[n];
    },
    indexWord32Arrayzh : function (a, n) {
        return new Uint32Array(a[0])[n];
    },
    indexWord64Arrayzh : function (a, n) {
        return new Uint64Array(a[0])[n];
    },
    readCharArrayzh : function (a, n, s) {
        return [s, String.fromCharCode(new Uint8Array(a[0])[n])];
    },
    readWideCharArrayzh : function (a, n, s) {
        return [s, String.fromCharCode(new Uint32Array(a[0])[n])];
    },
    readIntArrayzh : function (a, n, s) {
        return [s, new Int32Array(a[0])[n]];
    },
    readWordArrayzh : function (a, n, s) {
        return [s, new Uint32Array(a[0])[n]];
    },
    readAddrArrayzh : function (a, n, s) {
        var res = a[0].ptrs[n];
        if(HS_DEBUG && res[2] !== new Uint32Array(a[0])[n])
            throw "Array Pointer Error";
        return [s, res];
    },
    readFloatArrayzh : function (a, n, s) {
        return [s, Float32Array(a[0])[n]];
    },
    readDoubleArrayzh : function (a, n, s) {
        return [s, Float64Array(a[0])[n]];
    },
    readStablePtrArrayzh : function (a, n, s) {
        var res = a[0].ptrs[n];
        if(HS_DEBUG && res[2] !== new Uint32Array(a[0])[n])
            throw "Array Pointer Error";
        return [s, res];
    },
    readInt8Arrayzh : function (a, n, s) {
        return [s, new Int8Array(a[0])[n]];
    },
    readInt16Arrayzh : function (a, n, s) {
        return [s, new Int16Array(a[0])[n]];
    },
    readInt32Arrayzh : function (a, n, s) {
        return [s, new Int32Array(a[0])[n]];
    },
    readInt64Arrayzh : function (a, n, s) {
        return [s, new Int64Array(a[0])[n]];
    },
    readWord8Arrayzh : function (a, n, s) {
        return [s, new Uint8Array(a[0])[n]];
    },
    readWord16Arrayzh : function (a, n, s) {
        return [s, new Uint16Array(a[0])[n]];
    },
    readWord32Arrayzh : function (a, n, s) {
        return [s, new Uint32Array(a[0])[n]];
    },
    readWord64Arrayzh : function (a, n, s) {
        return [s, new Uint64Array(a[0])[n]];
    },
    writeCharArrayzh : function (a, n, v, s) {
        new Uint8Array(a[0])[n] = v.charCodeAt();
        return s;
    },
    writeWideCharArrayzh : function (a, n, v, s) {
        new Uint32Array(a[0])[n] = v.charCodeAt();
        return s;
    },
    writeIntArrayzh : function (a, n, v, s) {
        new Int32Array(a[0])[n] = v;
        return s;
    },
    writeWordArrayzh : function (a, n, v, s) {
        new Uint32Array(a[0])[n] = v;
        return s;
    },
    writeAddrArrayzh : function (a, n, v, s) {
        a[0].ptrs[n] = v;
        new Uint32Array(a[0])[n] = v[2];
        return s;
    },
    writeFloatArrayzh : function (a, n, v, s) {
        Float32Array(a[0])[n] = v;
        return s;
    },
    writeDoubleArrayzh : function (a, n, v, s) {
        Float64Array(a[0])[n] = v;
        return s;
    },
    writeStablePtrArrayzh : function (a, n, v, s) {
        a[0].ptrs[n] = v;
        new Uint32Array(a[0])[n] = v[2];
        return s;
    },
    writeInt8Arrayzh : function (a, n, v, s) {
        new Int8Array(a[0])[n] = v;
        return s;
    },
    writeInt16Arrayzh : function (a, n, v, s) {
        new Int16Array(a[0])[n] = v;
        return s;
    },
    writeInt32Arrayzh : function (a, n, v, s) {
        new Int32Array(a[0])[n] = v;
        return s;
    },
    writeInt64Arrayzh : function (a, n, v, s) {
        new Int64Array(a[0])[n] = v;
        return s;
    },
    writeWord8Arrayzh : function (a, n, v, s) {
        new Uint8Array(a[0])[n] = v;
        return s;
    },
    writeWord16Arrayzh : function (a, n, v, s) {
        new Uint16Array(a[0])[n] = v;
        return s;
    },
    writeWord32Arrayzh : function (a, n, v, s) {
        new Uint32Array(a[0])[n] = v;
        return s;
    },
    writeWord64Arrayzh : function (a, n, v, s) {
        new Uint64Array(a[0])[n] = v;
        return s;
    },
    copyByteArrayzh : function (src, soff, dest, doff, count, s) {
        srcarray = new Uint8Array(src[0]);
        destarray = new Uint8Array(dest[0]);
        while(count != 0) {
            destarray[doff] = srcarray[soff];
            soff++;
            doff++;
            count--;
        }
        return s;
    },
    copyMutableByteArrayzh : function (src, soff, dest, doff, count, s) {
        srcarray = new Uint8Array(src[0]);
        destarray = new Uint8Array(dest[0]);
        while(count != 0) {
            destarray[doff] = srcarray[soff];
            soff++;
            doff++;
            count--;
        }
        return s;
    },
    plusAddrzh : function (a, n) {
        return [a[0],a[1]+n,a[2]+n];
    },
    minusAddrzh : function (a, b) {
        return a[1]-b[1];
    },
    remAddrzh : function (a, b) {
        return a[1]%b;
    },
    gtAddrzh : function (a, b) {
        return a[2]>b[2] ? $$GHCziTypes_True:$$GHCziTypes_False;
    },
    geAddrzh : function (a, b) {
        return a[2]>=b[2] ? $$GHCziTypes_True:$$GHCziTypes_False;
    },
    eqAddrzh : function (a, b) {
        return a[2]===b[2] ? $$GHCziTypes_True:$$GHCziTypes_False;
    },
    neAddrzh : function (a, b) {
        return a[2]!==b[2] ? $$GHCziTypes_True:$$GHCziTypes_False;
    },
    ltAddrzh : function (a, b) {
        return a[2]<b[2] ? $$GHCziTypes_True:$$GHCziTypes_False;
    },
    leAddrzh : function (a, b) {
        return a[2]<=b[2] ? $$GHCziTypes_True:$$GHCziTypes_False;
    },
    indexCharOffAddrzh : function (a, n) {
        return String.fromCharCode(new Uint8Array(a[0],a[1]+n)[0]);
    },
    indexWideCharOffAddrzh : function (a, n) {
        return String.fromCharCode(new Uint32Array(a[0],a[1]+n*4)[0]);
    },
    indexIntOffAddrzh : function (a, n) {
        return new Int32Array(a[0],a[1]+n*4)[0];
    },
    indexWordOffAddrzh : function (a, n) {
        return new Uint32Array(a[0],a[1]+n*4)[0];
    },
    indexAddrOffAddrzh : function (a, n) {
        var res = a.ptrs[a[1]+n*4];
        if(HS_DEBUG && res[2] !== new Uint32Array(a[0],a[1]+n*4)[0])
            throw "Array Pointer Error";
        return res;
    },
    indexFloatOffAddrzh : function (a, n) {
        return Float32Array(a[0],a[1]+n*4)[0];
    },
    indexDoubleOffAddrzh : function (a, n) {
        return Float64Array(a[0],a[1]+n*8)[0];
    },
    indexStablePtrOffAddrzh : function (a, n) {
        var res = a.ptrs[a[1]+n*4];
        if(HS_DEBUG && res[2] !== new Uint32Array(a[0],a[1]+n*4)[0])
            throw "Array Pointer Error";
        return res;
    },
    indexInt8OffAddrzh : function (a, n) {
        return new Int8Array(a[0],a[1]+n)[0];
    },
    indexInt16OffAddrzh : function (a, n) {
        return new Int16Array(a[0],a[1]+n*2)[0];
    },
    indexInt32OffAddrzh : function (a, n) {
        return new Int32Array(a[0],a[1]+n*4)[0];
    },
    indexInt64OffAddrzh : function (a, n) {
        return new Int64Array(a[0],a[1]+n*8)[0];
    },
    indexWord8OffAddrzh : function (a, n) {
        return new Uint8Array(a[0],a[1]+n)[0];
    },
    indexWord16OffAddrzh : function (a, n) {
        return new Uint16Array(a[0],a[1]+n*2)[0];
    },
    indexWord32OffAddrzh : function (a, n) {
        return new Uint32Array(a[0],a[1]+n*4)[0];
    },
    indexWord64OffAddrzh : function (a, n) {
        return new Uint64Array(a[0],a[1]+n*8)[0];
    },
    readCharOffAddrzh : function (a, n, s) {
        return [s, String.fromCharCode(new Uint8Array(a[0],a[1]+n)[0])];
    },
    readWideCharOffAddrzh : function (a, n, s) {
        return [s, String.fromCharCode(new Uint32Array(a[0],a[1]+n*4)[0])];
    },
    readIntOffOffAddrzh : function (a, n, s) {
        return [s, new Int32Array(a[0],a[1]+n*4)[0]];
    },
    readWordOffOffAddrzh : function (a, n, s) {
        return [s, new Uint32Array(a[0],a[1]+n*4)[0]];
    },
    readAddrOffAddrzh : function (a, n, s) {
        var res = a.ptrs[a[1]+n*4];
        if(HS_DEBUG && a[2] !== new Uint32Array(a[0],a[1]+n*4)[0])
            throw "Array Pointer Error";
        return [s, res];
    },
    readFloatOffAddrzh : function (a, n, s) {
        return [s, Float32Array(a[0],a[1]+n*4)[0]];
    },
    readDoubleOffAddrzh : function (a, n, s) {
        return [s, Float64Array(a[0],a[1]+n*8)[0]];
    },
    readStablePtrOffAddrzh : function (a, n, s) {
        var res = a.ptrs[a[1]+n*4];
        if(HS_DEBUG && a[2] !== new Uint32Array(a[0],a[1]+n*4)[0])
            throw "Array Pointer Error";
        return [s, res];
    },
    readInt8OffAddrzh : function (a, n, s) {
        return [s, new Int8Array(a[0],a[1]+n)[0]];
    },
    readInt16OffAddrzh : function (a, n, s) {
        return [s, new Int16Array(a[0],a[1]+n*2)[0]];
    },
    readInt32OffAddrzh : function (a, n, s) {
        return [s, new Int32Array(a[0],a[1]+n*4)[0]];
    },
    readInt64OffAddrzh : function (a, n, s) {
        return [s, new Int64Array(a[0],a[1]+n*8)[0]];
    },
    readWord8OffAddrzh : function (a, n, s) {
        return [s, new Uint8Array(a[0],a[1]+n)[0]];
    },
    readWord16OffAddrzh : function (a, n, s) {
        return [s, new Uint16Array(a[0],a[1]+n*2)[0]];
    },
    readWord32OffAddrzh : function (a, n, s) {
        return [s, new Uint32Array(a[0],a[1]+n*4)[0]];
    },
    readWord64OffAddrzh : function (a, n, s) {
        return [s, new Uint64Array(a[0],a[1]+n*8)[0]];
    },
    writeCharOffAddrzh : function (a, n, v, s) {
        new Uint8Array(a[0],a[1]+n)[0] = v.charCodeAt();
        return s;
    },
    writeWideCharOffAddrzh : function (a, n, v, s) {
        new Uint32Array(a[0],a[1]+n*4)[0] = v.charCodeAt();
        return s;
    },
    writeIntOffAddrzh : function (a, n, v, s) {
        new Int32Array(a[0],a[1]+n*4)[0] = v;
        return s;
    },
    writeWordOffAddrzh : function (a, n, v, s) {
        new Uint32Array(a[0],a[1]+n*4)[0] = v;
        return s;
    },
    writeAddrOffAddrzh : function (a, n, v, s) {
        a.ptrs[n] = v;
        new Uint32Array(a[0],a[1]+n*4)[0] = a[2];
        return s;
    },
    writeFloatOffAddrzh : function (a, n, v, s) {
        Float32Array(a[0],a[1]+n*4)[0] = v;
        return s;
    },
    writeDoubleOffAddrzh : function (a, n, v, s) {
        Float64Array(a[0],a[1]+n*8)[0] = v;
        return s;
    },
    writeStablePtrOffAddrzh : function (a, n, v, s) {
        a.ptrs[n] = v;
        new Uint32Array(a[0],a[1]+n*4)[0] = a[2];
        return s;
    },
    writeInt8OffAddrzh : function (a, n, v, s) {
        new Int8Array(a[0],a[1]+n)[0] = v;
        return s;
    },
    writeInt16OffAddrzh : function (a, n, v, s) {
        new Int16Array(a[0],a[1]+n*2)[0] = v;
        return s;
    },
    writeInt32OffAddrzh : function (a, n, v, s) {
        new Int32Array(a[0],a[1]+n*4)[0] = v;
        return s;
    },
    writeInt64OffAddrzh : function (a, n, v, s) {
        new Int64Array(a[0],a[1]+n*8)[0] = v;
        return s;
    },
    writeWord8OffAddrzh : function (a, n, v, s) {
        new Uint8Array(a[0],a[1]+n)[0] = v;
        return s;
    },
    writeWord16OffAddrzh : function (a, n, v, s) {
        new Uint16Array(a[0],a[1]+n*2)[0] = v;
        return s;
    },
    writeWord32OffAddrzh : function (a, n, v, s) {
        new Uint32Array(a[0],a[1]+n*4)[0] = v;
        return s;
    },
    writeWord64OffAddrzh : function (a, n, v, s) {
        new Uint64Array(a[0],a[1]+n*8)[0] = v;
        return s;
    },

    // Propper weak pointers will be hard and probably slow.
    mkWeakzh : function(o, b, c, s) { return [s, b]; },
    mkWeakForeignEnvzh : function(o, b, w, x, y, z, s) { return [s, b]; },
    deRefWeakzh : function(p, s) { return [s, 1, p]; },
    finalizeWeakzh : function(p, s) { return [s, 0, null]; },
    touchzh : function(a, s) { return s; }
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
};

var _hs_text_memcmp = function(a, aoff, b, boff, count) {
    aarray = new Uint16Array(a[0],a[1]);
    barray = new Uint16Array(b[0],b[1]);
    while(count != 0) {
        if( aarray[aoff] < barray[boff] )
            return -1;
        if( aarray[aoff] > barray[boff] )
            return 1;
        soff++;
        doff++;
        count--;
    }
    return 0;
};

var u_iswspace = function(a) { return 0; };
var u_iswalpha = function(a) { return 0; };
var u_towupper = function(a) { return String.fromCharCode(a).toUpperCase().charCodeAt(); };
var rtsSupportsBoundThreads = function () { return 0; };
var getOrSetGHCConcSignalSignalHandlerStore = function (x) { return x; };
var stg_sig_install = function(a,b,c) { return -1; };
var localeEncoding = function() { return $hs.ascii("UTF-32LE"); };
var hs_iconv_open = function(to,from) { return 1; };
var hs_iconv_close = function(h) { return 0; };

var errno = 0;
var __hscore_get_errno = function() { return 0; }
var __hscore_set_errno = function(e) { errno = e; };

var __hscore_s_isreg = function(m) { return 1; };
var __hscore_s_isdir = function(m) { return 0; };
var __hscore_s_isfifo = function(m) { return 0; };
var __hscore_s_isblk = function(m) { return 0; };
var __hscore_s_ischr = function(m) { return 0; };
var __hscore_s_issock = function(m) { return 0; };

var __hscore_sigemptyset = function(set) { return 0; };
var __hscore_sigfillset = function(set) { return 0; };
var __hscore_sigaddset = function(set, s) { return 0; };
var __hscore_sigdelset = function(set, s) { return 0; };
var __hscore_sigismember = function(set, s) { return 0; };

var __hscore_memcpy_src_off = function(dest, src, soff, count)
{
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

var __hscore_ftruncate = function(fd, where) { return 0; };
var __hscore_setmode = function(fd, toBin) { return 0; };

var __hscore_sizeof_stat = function() { return 0; };
var __hscore_st_mtime = function(st) { return 0; };
var __hscore_st_size = function(st) { return 0; };
var __hscore_st_mode = function(st) { return 0; };
var __hscore_st_dev = function(st) { return 0; };
var __hscore_st_ino = function(st) { return 0; };
var __hscore_stat = function(file, buf) { return 0; };
var __hscore_fstat = function(fd, buf) { return 0; };
var __hscore_lstat = function(fname, buf) { return 0; };

var __hscore_lflag = function(ts) { return ts.c_lflag; };
var __hscore_poke_lflag = function(ts, t) { ts.c_lflag = t; };
var __hscore_ptr_c_cc = function(ts) { return ts.c_cc; };
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
var __hscore_get_saved_termios = function(fd) { return null; };
var __hscore_set_saved_termios = function(fd, ts) {};
var __hscore_hs_fileno = function(f) { return 0; };

var __hscore_open = function(f,h,m) {
    return 0;
};
var __hscore_lseek = function(fd, off, whence) { return 0; };

var hsFD_SETSIZE = function() { return 1024; };
var hsFD_ISSET = function(fd, fds) {
    return 0;
};
var hsFD_SET = function(fd, fds) {};
var sizeof_fd_set = function() {
    return 0;
};
var hsFD_ZERO = function(fds) {
};

var __hscore_select = function(nfds, readfds, writefds, exceptfds, timeout) {
    return 0;
};
var environ = {};
var __hscore_environ = function() { return environ; };

var lockFile = function(fd, dev, ino, for_writing) { return 0; };
var unlockFile = function(fd) { return 0; };
var isatty = function(fd) { return 0; };
var read = function(fd, p, s) { return 0; };

$hs.utf32 = function(s) {
    res = $hs.newByteArrayzh(s.length*4+4);
    var dest = new Uint32Array(res[0]);
    for(i=0;i!=s.length;i++)
        dest[i]=s.charCodeAt(i);
    return [res, 0];
};
$hs.fromUtf32 = function(s) {
    var res = "";
    var src = new Uint32Array(s[0],s[1]);
    for(i=0;i!=src.length;i++)
        res=res+String.fromCharCode(src[i]);
    return res;
};
$hs.ascii = function(s) {
    var res = new ArrayBuffer(s.length+1);
    var dest = new Uint8Array(res);
    for(i=0;i!=s.length;i++)
        dest[i]=s.charCodeAt(i);
    return [res, 0];
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
                    goog.globalEval(transport.responseText);
                    $hs.loaded[modules[i]] = true;
                } catch (e) {
                    $hs.logError("Error evaluating module: " + path + ":\n" + e);
                    return false;
                }
            }
        }
    }
};

$hs.fromHaskellString = function() {
  var res = "";
  var s = $hs.force.apply($hs, arguments);
  for (;;) {
    switch (s.g) {
      case 1: // nil
        return res;
      case 2: // cons
        var chthunk = s.v[0];
        var sthunk = s.v[1];
        var ch = $hs.force(chthunk);
        res = res + ch.v[0];
        s = $hs.force(sthunk);
        break;
      default:
        return undefined;
    }
  }
};
$hs.fromHaskellInt = function() {
    console.log("fromHaskellInt");
    var i = $hs.force.apply($hs, arguments);
    return i.v[0];
};
$hs.fromHaskellIO = function() {
    console.log("fromHaskellIO");
    var newArguments = [];
    for (var i = 0; i < arguments.length; i++)
    newArguments[i] = arguments[i];
    newArguments[arguments.length] = $$GHCziPrim_realWorldzh;
    var i = $hs.force.apply($hs, newArguments);
    return i[1];
};
$hs.toHaskellInt = function(i) {
    console.log("toHaskellInt");
    return new $DataValue(1, [(0 + i) & ~0]);
};
$hs.nil = function() {
    return "";
};
$hs.cons = function(x, xs) {
    return String.fromCharCode(x) + xs;
};
$hs.init = function() {
    $$GHCziPrim_realWorldzh = $D(1, function(){return []}, "realWorld#");
    $$GHCziPrim_coercionTokenzh = $D(1, function(){return []}, "coercionToken#");
}
