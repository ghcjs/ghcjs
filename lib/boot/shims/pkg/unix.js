function h$mkdir(dir_d, dir_o, mode) {
#ifndef GHCJS_BROWSER
  if(h$isNode) {
    return h$handleErrno(-1, function() {
      h$fs.mkdirSync(h$decodeUtf8z(dir_d, dir_o), mode);
      return 0;
     });
  } else
#endif
    return h$unsupported(-1);
}
