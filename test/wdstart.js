var args     = arguments[0];
var root     = arguments[1];
var callback = arguments[arguments.length-1];
function startWD() {
  if(typeof window.h$runMainWebDriver !== 'undefined') {
    window.h$runMainWebDriver(args, root, callback);
  } else {
    setTimeout(startWD, 10);
  }
}
startWD();
