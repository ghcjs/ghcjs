function putchar(ch) {
  var str = String.fromCharCode(ch);
  if(typeof(process) !== 'undefined') {
    process.stdout.write(str);
  } else if(typeof(putstr) !== 'undefined') {
    putstr(str);
  } else if(typeof(console) !== 'undefined') {
    console.log(str);
  }
}
