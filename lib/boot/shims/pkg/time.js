function h$get_current_timezone_seconds(t, pdst_v, pdst_o, pname_v, pname_o) {
    var d      = new Date(t * 1000);
    var now    = new Date();
    var jan    = new Date(now.getFullYear(),0,1);
    var jul    = new Date(now.getFullYear(),6,1);
    var stdOff = Math.max(jan.getTimezoneOffset(), jul.getTimezoneOffset());
    var isDst  = d.getTimezoneOffset() < stdOff;
    var tzo    = d.getTimezoneOffset();
    pdst_v.dv.setInt32(pdst_o, isDst ? 1 : 0, true);
    if(!pname_v.arr) pname_v.arr = [];
    var offstr = tzo < 0 ? ('+' + (tzo/-60)) : ('' + (tzo/-60));
    pname_v.arr[pname_o] = [h$encodeUtf8("UTC" + offstr), 0];
    return (-60*tzo)|0;
}

function h$clock_gettime(when, p_d, p_o) {
/*  h$log("clock_gettime");
  h$log(when);
  h$log(p_d);
  h$log(p_o); */

  var o  = p_o >> 2,
      t  = Date.now ? Date.now() : new Date().getTime(),
      tf = Math.floor(t / 1000),
      tn = 1000000 * (t - (1000 * tf));
  p_d.i3[o]   = tf|0;
  p_d.i3[o+1] = tn|0;
  return 0;
}
