window.onload = function(){
  goog.debug.Console.autoInstall();
  $hs_loadPath = "./";

  // Must be called first
  $hs_init();
  $hs_htermInitAndRunIO([$$$Main_main]);
}

