// make sure we never exit the renderer, since this closes our debugging connection
process.exit = function(code) {
  console.warn("exit process with code: " + code);
}

function Fingerprint2() {
  console.log("Fingerprint2 created");
}

Fingerprint2.prototype.get = function(f) {
  f("fingerprint", ["finger", "print"]);
}
