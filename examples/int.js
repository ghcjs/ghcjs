var Word16 = {}
Word16.addCarry = function(a, b, c) {
  var sum = a + b + c;
  var res = sum & 0xFFFF;
  var carry = sum >> 16;
  return [res, carry];
}
var Word32 = {}
Word32.addCarry = function(a, b, c) {
  var sum = a + b + c;
  var res = sum & 0xFFFFFFFF;
  var carry = sum >> 32;
  return [res, carry];
}

var Int32 = {}
Int32.addCarry = function(a, b, c) {
  var resl = Word16.addCarry(a & 0xFFFF, b & 0xFFFF, c);
  var resh = Word16.addCarry(a >> 16, b >> 16, resl[1]);
  return [(resh[0] << 16) | resl[0], resh[1]];
}
Int32.addC = function(a, b) { return Int32.addCarry(a, b, 0); }
Int32.subC = function(a, b) { return Int32.addCarry(a, b ^ 0xFFFFFFFF, 1); }
Int32.add = function(a, b) { return Int32.addC(a, b)[0]; }
Int32.sub = function(a, b) { return Int32.subC(a, b)[0]; }
Int32.mul = function(a, b) {
  var al = a & 0xFFFF;
  var ah = a >> 16;
  var bl = a & 0xFFFF;
  var bh = b >> 16;
  var r = Int32.add(al * bh, bl * ah)
  return Int32.add(al * bl, (r & 0xFFFF) << 16);
}
Int32.mulIntMayOflo = function(a, b) {
  return a >> 16 == 0 && b >> 16 == 0;
}
Int32.quot = function(a, b) {
  return (a - a % b) / b;
}
Int32.rem = function(a, b) {
  return a % b;
}

var Int64 = {}
Int64.addCarry = function(a, b, c) {
  var resl = Word32.addCarry(a & 0xFFFFFFFF, b & 0xFFFFFFFF, c);
  var resh = Word32.addCarry(a >> 32, b >> 32, resl[1]);
  return [(resh[0] << 16) | resl[0], resh[1]];
}
Int64.addC = function(a, b) { return Int64.addCarry(a, b, 0); }
Int64.subC = function(a, b) { return Int64.addCarry(a, b ^ 0xFFFFFFFF, 1); }
Int64.add = function(a, b) { return Int64.addC(a, b)[0]; }
Int64.sub = function(a, b) { return Int64.subC(a, b)[0]; }
Int64.mul = function(a, b) {
  var al = a & 0xFFFFFFFF;
  var ah = a >> 32;
  var bl = a & 0xFFFFFFFF;
  var bh = b >> 32;
  var r = Int64.add(al * bh, bl * ah)
  return Int64.add(al * bl, (r & 0xFFFFFFFF) << 32);
}
Int64.mulIntMayOflo = function(a, b) {
  return a >> 32 == 0 && b >> 32 == 0;
}
Int64.quot = function(a, b) {
  return (a - a % b) / b;
}
Int64.rem = function(a, b) {
  return a % b;
}

