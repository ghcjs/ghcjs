var testLog;

var logResult = function(a) {
    testLog.innerHTML = testLog.innerHTML + "<p>" + a + "</p>"
};

var logException = function(e) {
    testLog.innerHTML = testLog.innerHTML + "<p>Error " + e + "</p>"
};

window.onload = function() {
    $hs_init();
    try {
      testLog = document.getElementById("log");

      $hs_fromInt([$$$Test_test1], logResult, logException);
      $hs_fromInt([$$$Test_test2], logResult, logException);
      $hs_fromInt([$$$Test_test3], logResult, logException);
      $hs_fromString([$$$Test_test4], logResult, logException);
      $hs_fromString([$$$Test_test5], logResult, logException);
      $hs_fromString([$$$Test_test6], logResult, logException);
      $hs_fromString([$$$Test_test7], logResult, logException);
      $hs_runIO([$$$Test_test9], function(i) {
        $hs_fromInt([i], logResult, logException);}, logException);

      if ($tr_Thread && $tr_MVar) {
        $hs_runIO([$$$Test_test10], function(i) {
            $hs_fromInt([i], logResult, logException);}, logException);
      }
      var s42 = "42";
      $hs_force([$$$Test_test11, s42], function(i) {
        $hs_fromInt([i], logResult, logException);}, logException);
      $hs_fromIO([$$$Test_test12, s42], function(i) {
        $hs_fromInt([i], logResult, logException);}, logException);
      $hs_force([$$$Test_test13, s42], function(s) {
        $hs_fromString([s], logResult, logException);}, logException);
    } catch (e) {
      alert(e);
    }
};
