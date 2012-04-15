var testLog;

logResult = function(a) {
    testLog.innerHTML = testLog.innerHTML + "<p>" + a + "</p>"
};

window.onload = function() {
    $hs.init();
    try {
      testLog = document.getElementById("log");

      logResult($hs.fromHaskellInt($$$Test_test1()));
      logResult($hs.fromHaskellInt($$$Test_test2()));
      logResult($hs.fromHaskellInt($$$Test_test3()));
      logResult($hs.fromHaskellString($$$Test_test4()));
      logResult($hs.fromHaskellString($$$Test_test5()));
      logResult($hs.fromHaskellString($$$Test_test6()));
      logResult($hs.fromHaskellString($$$Test_test7()));
      logResult($hs.fromHaskellInt($hs.fromHaskellIO($$$Test_test9())));

      if ($hs.Thread && $hs.MVar) {
        logResult("Thread test returned " +
            $hs.fromHaskellInt($hs.fromHaskellIO($$$Test_test10())));
      }

      logResult($hs.fromHaskellInt($hs.force($$$Test_test11(),
        $hs.force($$GHCziCString_unpackCStringzh, "42\x00"))));
      logResult($hs.fromHaskellInt($hs.fromHaskellIO($hs.force($$$Test_test12(),
        $hs.force($$GHCziCString_unpackCStringzh, "42\x00")))));
      logResult($hs.fromHaskellString($hs.force($$$Test_test13(),
        $hs.force($$GHCziCString_unpackCStringzh, "1\x00"))));
    } catch (e) {
      alert(e);
    }
};
