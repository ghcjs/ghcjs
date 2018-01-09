#include <ghcjs/rts.h>

// static pointers
var h$static_pointer_table      = null;
var h$static_pointer_table_keys = null;

function h$hs_spt_insert(key1,key2,key3,key4,ref) {
    // h$log("hs_spt_insert: " + key1 + " " + key2 + " " + key3 + " " + key4 + " -> " + h$collectProps(ref));
    if(!h$static_pointer_table) {
	h$static_pointer_table      = [];
	h$static_pointer_table_keys = [];
    }
    if(!h$hs_spt_lookup_key(key1,key2,key3,key4)) {
        var ba = h$newByteArray(16);
        ba.i3[0] = key1;
        ba.i3[1] = key2;
        ba.i3[2] = key3;
        ba.i3[3] = key4;
	h$static_pointer_table_keys.push([ba,0]);
        h$retain({ root: ref, _key: -1 });
    }
    var s = h$static_pointer_table;
    if(!s[key1])             s[key1]             = [];
    if(!s[key1][key2])       s[key1][key2]       = [];
    if(!s[key1][key2][key3]) s[key1][key2][key3] = [];
    s[key1][key2][key3][key4] = ref;
}

function h$hs_spt_key_count() {
    return h$static_pointer_table_keys ?
              h$static_pointer_table_keys.length : 0;
}

function h$hs_spt_keys(tgt_d, tgt_o, n) {
    var ks = h$static_pointer_table_keys;
    if(!tgt_d.arr) tgt_d.arr = [];
    for(var i=0;(i<n&&i<ks.length);i++) tgt_d.arr[tgt_o+4*i] = ks[i];
    return Math.min(n,ks.length);
}

function h$hs_spt_lookup(key_d, key_o) {
    var i3 = key_d.i3, o = key_o >> 2;
    RETURN_UBX_TUP2(h$hs_spt_lookup_key(i3[o],i3[o+1],i3[o+2],i3[o+3]), 0);
}

function h$hs_spt_lookup_key(key1,key2,key3,key4) {
    var s = h$static_pointer_table;
    if(s && s[key1] && s[key1][key2] && s[key1][key2][key3] &&
       s[key1][key2][key3][key4]) return s[key1][key2][key3][key4];
    return null;
}
