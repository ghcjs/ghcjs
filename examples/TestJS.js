      window.onload = function() {
         // URLs to load modules from in order of precedence

      //  $hs.loadPaths = ["."];

        // Used packages
       // $hs.packages = ["/main", "/ghc-prim", "/integer-simple", "/base"];

        // Must be called first
        $hs.init();

      //  $hs.loadModule("Test");

        try {

          alert("Test");
          if ($hs.Thread && $hs.MVar) {
            res = $hs.fromHaskellInt($hs.fromHaskellIO($$Test_test10));
            alert("Thread test returned " + res);
          }

          var res = $hs.fromHaskellInt($$Test_test1);
          alert(res);

          var res = $hs.fromHaskellInt($$Test_test2);
          alert(res);

          var res = $hs.fromHaskellInt($$Test_test3);
          alert(res);

          var res = $hs.fromHaskellString($$Test_test4);
          alert(res);

          var res = $hs.fromHaskellString($$Test_test5);
          alert(res);

          var res = $hs.fromHaskellString($$Test_test6);
          alert(res);

          var res = $hs.fromHaskellString($$Test_test7);
          alert(res);

          var res = $hs.fromHaskellInt($hs.fromHaskellIO($$Test_test9));
          alert(res);

        } catch (e) {
          alert(e);
        }
}
