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

      $hs_fromHaskellInt([$$$Test_test1()], logResult, logException);
      $hs_fromHaskellInt([$$$Test_test2()], logResult, logException);
      $hs_fromHaskellInt([$$$Test_test3()], logResult, logException);
      $hs_fromHaskellString([$$$Test_test4()], logResult, logException);
      $hs_fromHaskellString([$$$Test_test5()], logResult, logException);
      $hs_fromHaskellString([$$$Test_test6()], logResult, logException);
      $hs_fromHaskellString([$$$Test_test7()], logResult, logException);
      $hs_fromHaskellIO([$$$Test_test9()], function(i) {
        $hs_fromHaskellInt([i], logResult, logException);}, logException);

      if ($tr_Thread && $tr_MVar) {
        $hs_fromHaskellIO([$$$Test_test10()], function(i) {
            $hs_fromHaskellInt([i], logResult, logException);}, logException);
      }
      $hs_force([$$GHCziCString_unpackCStringzh, "42\x00"], function(s42) {
        $hs_force([$$$Test_test11(), s42], function(i) {
            $hs_fromHaskellInt([i], logResult, logException);}, logException);
        $hs_fromHaskellIO([$$$Test_test12(), s42], function(i) {
            $hs_fromHaskellInt([i], logResult, logException);}, logException);
        $hs_force([$$$Test_test13(), s42], function(s) {
            $hs_fromHaskellString([s], logResult, logException);}, logException);
      }, logException);
    } catch (e) {
      alert(e);
    }
};
