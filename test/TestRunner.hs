{-# LANGUAGE CPP, FlexibleContexts, OverloadedStrings, TupleSections, ScopedTypeVariables, ExtendedDefaultRules, LambdaCase #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Lens (over, _1)
import           Control.Concurrent.MVar
import           Control.Concurrent
import           Data.Char (isLower, toLower, isDigit, isSpace)
import           Data.IORef
import qualified Data.HashMap.Strict as HM
import           Data.List (partition, isPrefixOf, isInfixOf)
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable (sequenceA)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Clock (getCurrentTime, diffUTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Traversable (traverse)
import           System.FilePath (dropExtensions, addExtension, replaceExtensions, takeDirectory, takeFileName, replaceExtension, isExtensionOf, takeExtension, takeBaseName)
import           Shelly
import           System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, findExecutable, copyFile, removeDirectoryRecursive)
import           System.Environment (getArgs, getEnv)
import           System.Exit
import           System.IO -- hiding (FilePath)
import           System.IO.Error
import           System.Process ( createProcess, proc, CreateProcess(..), StdStream(..)
                                , terminateProcess, waitForProcess, readProcessWithExitCode
                                , ProcessHandle )
import           System.Random (randomRIO)
import           System.Timeout (timeout)
import           Test.Framework
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit.Base (assertBool, assertFailure, assertEqual, Assertion)
import           Test.HUnit.Lang (HUnitFailure(..), formatFailureReason)
import qualified Data.Yaml as Yaml
import           Data.Yaml (FromJSON(..), Value(..), (.:), (.:?), (.!=))
import           Data.Default
import           Foreign.C.Error (ePIPE, Errno(..))
import           Control.DeepSeq
import           GHC.IO.Exception(IOErrorType(..), IOException(..))
import qualified Control.Exception as C
import           Text.Read (readMaybe)
import           Options.Applicative
import           Options.Applicative.Common
import           Options.Applicative.Types
import           Options.Applicative.Internal
import           Options.Applicative.Help hiding ((</>), fullDesc)
import qualified Options.Applicative.Help as H

import qualified Server
import qualified Client
import           Types

default (Text)

-- | path containing the test cases and data files
getTestDir :: FilePath -> IO FilePath
#ifdef STANDALONE
getTestDir ghcjs = do
  (ec, libDir, _) <- readProcessWithExitCode (encodeString ghcjs) ["--print-libdir"] ""
  when (ec /= ExitSuccess) (error "could not determine GHCJS installation directory")
  let testDir = fromString (trim libDir) </> "test"
  e <- doesFileExist (encodeString $ testDir </> "tests.yaml")
  when (not e) (error $ "test suite not found in " ++ testDir ++ ", GHCJS might have been installed without tests")
  return testDir
#else
getTestDir _ = do
  testDir <- (</> "test") . fromString <$> getCurrentDirectory
  e <- doesFileExist (testDir </> "tests.yaml")
  when (not e) (error $ "test suite not found in " ++ testDir)
  return testDir
#endif

main :: IO ()
main = shellyE . silently . withTmpDir $ liftIO . setupTests

showParseError :: ParseError -> String
showParseError (ErrorMsg xs)          = "error: " ++ xs
showParseError (InfoMsg xs)           = "info: " ++ xs
showParseError ShowHelpText           = "help!"
showParseError UnknownError           = "unknown"
showParseError (MissingError {})      = "missing"
showParseError (ExpectsArgError xs)   = "expecting argument: " ++ xs
showParseError (UnexpectedError xs _) = "unexpected: " ++ xs

setupTests :: FilePath -> IO ()
setupTests tmpDir = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getArgs
  (testArgs, leftoverArgs) <-
    -- case runP (runParser AllowOpts optParser args) (prefs idm) of
    case runP (runParser Intersperse CmdStart optParser args) (prefs idm) of
      (Left err, _ctx)    -> error ("error parsing arguments: " ++ showParseError err)
      (Right (a,l), _ctx) -> return (a,l)
  when (taHelp testArgs) $ do
    defaultMainWithArgs [] ["--help"] `C.catch` \(e::ExitCode) -> return ()
    putStrLn $ renderHelp 80 (parserHelp (prefs idm) optParser)
    exitSuccess
  let ghcjs      = fromString (taWithGhcjs testArgs)
      ghcjsPkg   = fromString (taWithGhcjsPkg testArgs)
      runhaskell = fromString (taWithRunhaskell testArgs)
  checkBooted ghcjs
  testDir       <- maybe (getTestDir ghcjs) (return . fromString) (taWithTests testArgs)
  nodePgm       <- checkProgram "node" (taWithNode testArgs)           ["--help"] ("Node.js" `isInfixOf`)
  smPgm         <- checkProgram "js"   (taWithSpiderMonkey testArgs)   ["--help"] ("JavaScript-C" `isInfixOf`)
  jscPgm        <- checkProgram "jsc"  (taWithJavaScriptCore testArgs) ["--help"] ("JSC VM" `isInfixOf`)
  -- fixme use command line options instead
  onlyOptEnv    <- getEnvOpt "GHCJS_TEST_ONLYOPT"
  onlyUnoptEnv  <- getEnvOpt "GHCJS_TEST_ONLYUNOPT"
  log           <- newIORef []
  let noProf    = taNoProfiling testArgs
      runDir    = tmpDir </> "run"
  createDirectoryIfMissing False runDir
  (wdRunMain, wdStart, wdHtml, browserSessions) <-
    startBrowserSessions testDir
                         runDir
                         (taSeleniumHost testArgs)
                         (taSeleniumPort testArgs)
  (symbs, base) <- prepareBaseBundle testDir ghcjs []
  (profSymbs, profBase) <- if noProf then return (symbs, base)
                                     else prepareBaseBundle testDir ghcjs ["-prof"]
  let specFile  = testDir </> if taBenchmark testArgs then "benchmarks.yaml" else "tests.yaml"
      symbsFile = tmpDir </> "base.symbs"
      profSymbsFile = tmpDir </> "base.p_symbs"

      disUnopt  = onlyOptEnv || taBenchmark testArgs
      disOpt    = onlyUnoptEnv
      opts      = TestOpts (onlyOptEnv || taBenchmark testArgs) onlyUnoptEnv noProf (taTravis testArgs) log testDir runDir
                              symbsFile base
                              profSymbsFile profBase
                              ghcjs runhaskell nodePgm smPgm jscPgm
                              wdRunMain wdStart wdHtml browserSessions
  es <- doesFileExist specFile
  when (not es) (error $ "test suite not found in " ++ testDir)
  ts <- B.readFile specFile >>=
          \x -> case Yaml.decodeEither x of
                       Left err -> error ("error in test spec file: " ++ specFile ++ "\n" ++ err)
                       Right t  -> return t
  groups <- forM (tsuiGroups ts) $ \(dir, name) ->
    testGroup name <$> allTestsIn opts testDir dir
  checkRequiredPackages (fromString $ taWithGhcjsPkg testArgs) (tsuiRequiredPackages ts)
  B.writeFile symbsFile symbs
  B.writeFile profSymbsFile profSymbs
  when (disUnopt && disOpt) (putStrLn "nothing to do, optimized and unoptimized disabled")
  putStrLn ("running tests in " <> testDir)
  (defaultMainWithArgs groups leftoverArgs `C.finally` closeBrowserSessions opts)
    `C.catch` \(e::ExitCode) -> do
      errs <- readIORef log
      when (e /= ExitSuccess && not (null errs))
              (putStrLn "\nFailed tests:" >> mapM_ putStrLn (reverse errs) >> putStrLn "")
      when (e /= ExitSuccess) (C.throwIO e)

startBrowserSessions :: FilePath
                     -> FilePath
                     -> Maybe String
                     -> Maybe Int
                     -> IO (B.ByteString, Text, B.ByteString, [Client.Session])
startBrowserSessions _ _ (Just "none") _ = return (mempty, mempty, mempty, [])
startBrowserSessions testRoot serverRoot mbSeleniumHost mbSeleniumPort = do
  let seleniumHost = fromMaybe "127.0.0.1" mbSeleniumHost
      seleniumPort = fromMaybe 4444        mbSeleniumPort
  wdRunMain  <- B.readFile (testRoot </> "wdrunmain.js")
  wdHtml     <- B.readFile (testRoot </> "wdindex.html")
  serverPort <- Server.startServer serverRoot -- wdScript
  wdStart    <- T.readFile (testRoot </> "wdstart.js")
  sess <- Client.startSessions (Client.Server serverPort wdStart)
                               (T.pack seleniumHost)
                               seleniumPort
  return (wdRunMain, wdStart, wdHtml, sess)

closeBrowserSessions :: TestOpts -> IO ()
closeBrowserSessions opts = mapM_ Client.closeSession (browsers opts)


checkBooted :: FilePath -> IO ()
checkBooted ghcjs = check `C.catch` \(e::C.SomeException) -> cantRun e
  where
    cantRun e = do
#ifdef STANDALONE
      putStrLn ("Error running GHCJS: " ++ show e)
      exitFailure
#else
      putStrLn ("Error running GHCJS, skipping tests:\n" ++ show e)
      exitSuccess
#endif
    check = do
      (ec, _, _) <- readProcessWithExitCode ghcjs ["-c", "x.hs"] ""
      case ec of
        (ExitFailure 87) -> do
          putStrLn "GHCJS is not booted, skipping tests"
          exitSuccess
        _ -> return ()

-- find programs at the start so we don't try to run a nonexistent program over and over again
-- temporary workaround, process-1.2.0.0 leaks when trying to run a nonexistent program
checkProgram :: FilePath -> Maybe String -> [String] -> (String -> Bool) -> IO (Maybe FilePath)
checkProgram _ (Just "none") _ _ = return Nothing
checkProgram defName userName testArgs testP = do
  let checkOutput (ExitFailure _, _, _) = False
      checkOutput (_, sout, serr)       = testP sout || testP serr
      testProg p as = either (\(e::C.SomeException) -> False) checkOutput <$>
                        C.try (readProcessWithExitCode' "/" p as "")
  findExecutable (fromMaybe defName userName) >>= \case
    Nothing | Just n <- userName -> error ("could not find program " ++ defName ++ " at " ++ n)
    Nothing                      -> return Nothing
    Just p ->
      testProg p testArgs >>= \case
        True  -> return (Just $ fromString p)
        False -> return Nothing

data TestArgs = TestArgs { taHelp               :: Bool
                         , taWithGhcjs          :: String
                         , taWithGhcjsPkg       :: String
                         , taWithRunhaskell     :: String
                         , taWithNode           :: Maybe String
                         , taWithSpiderMonkey   :: Maybe String
                         , taWithJavaScriptCore :: Maybe String
                         , taSeleniumPort       :: Maybe Int
                         , taSeleniumHost       :: Maybe String
                         , taWithTests          :: Maybe String
                         , taNoProfiling        :: Bool
                         , taBenchmark          :: Bool
                         , taTravis             :: Bool
                         } deriving Show

optParser :: Parser TestArgs
optParser = TestArgs <$> switch (long "help" <> help "show help message")
                     <*> strOption (long "with-ghcjs"      <> metavar "PROGRAM" <> value "ghcjs"      <> help "ghcjs program to use")
                     <*> strOption (long "with-ghcjs-pkg"  <> metavar "PROGRAM" <> value "ghcjs-pkg"  <> help "ghcjs-pkg program to use")
                     <*> strOption (long "with-runhaskell" <> metavar "PROGRAM" <> value "runhaskell" <> help "runhaskell program to use")
                     <*> (optional . strOption) (long "with-node" <> metavar "PROGRAM" <> help "node.js program to use")
                     <*> (optional . strOption) (long "with-spidermonkey" <> metavar "PROGRAM" <> help "SpiderMonkey jsshell program to use")
                     <*> (optional . strOption) (long "with-javascriptcore" <> metavar "PROGRAM" <> help "JavaScriptCore jsc program to use")
                     <*> (optional . option auto) (long "selenium-port" <> metavar "PORT" <> help "port number of Selenium server")
                     <*> (optional . strOption) (long "selenium-host" <> metavar "HOSTNAME" <> help "host name of Selenium server")
                     <*> (optional . strOption) (long "with-tests" <> metavar "LOCATION" <> help "location of the test cases")
                     <*> switch (long "no-profiling" <> help "do not run profiling tests")
                     <*> switch (long "benchmark" <> help "run benchmarks instead of regression tests")
                     <*> switch (long "travis" <> help "use settings for running on Travis CI")

-- settings for the test suite
data TestOpts = TestOpts { disableUnopt          :: Bool
                         , disableOpt            :: Bool
                         , noProfiling           :: Bool
                         , travisCI              :: Bool
                         , failedTests           :: IORef [String] -- yes it's ugly but i don't know how to get the data from test-framework
                         , testsuiteLocation     :: FilePath
                         , runDir                :: FilePath
                         , baseSymbs             :: FilePath
                         , baseJs                :: B.ByteString
                         , profBaseSymbs         :: FilePath
                         , profBaseJs            :: B.ByteString
                         , ghcjsProgram          :: FilePath
                         , runhaskellProgram     :: FilePath
                         , nodeProgram           :: Maybe FilePath
                         , spiderMonkeyProgram   :: Maybe FilePath
                         , javaScriptCoreProgram :: Maybe FilePath
                         , wdRunMain             :: B.ByteString
                         , wdStart               :: Text
                         , wdHtml                :: B.ByteString
                         , browsers              :: [Client.Session]
                         }

-- settings for a single test
data TestSettings =
  TestSettings { tsDisableNode           :: Bool
               , tsDisableSpiderMonkey   :: Bool
               , tsDisableJavaScriptCore :: Bool
               , tsDisableOpt            :: Bool
               , tsDisableUnopt          :: Bool
               , tsDisableTravis         :: Bool
               , tsDisabled              :: Bool
               , tsProf                  :: Bool     -- ^ use profiling bundle
               , tsBrowserOnly           :: Bool     -- ^ only run in browser
               , tsDisableBrowser        :: Bool
               , tsCompArguments         :: [String] -- ^ command line arguments to pass to compiler
               , tsArguments             :: [String] -- ^ command line arguments to pass to interpreter(node, js)
               , tsCopyFiles             :: [String] -- ^ copy these files to the dir where the test is run
               } deriving (Eq, Show)

instance Default TestSettings where
  def = TestSettings False False False False False False False False False False [] [] []

instance FromJSON TestSettings where
  parseJSON (Object o) = TestSettings <$> o .:? "disableNode"           .!= False
                                      <*> o .:? "disableSpiderMonkey"   .!= False
                                      <*> o .:? "disableJavaScriptCore" .!= False
                                      <*> o .:? "disableOpt"            .!= False
                                      <*> o .:? "disableUnopt"          .!= False
                                      <*> o .:? "disableTravis"         .!= False
                                      <*> o .:? "disabled"              .!= False
                                      <*> o .:? "prof"                  .!= False
                                      <*> o .:? "browserOnly"           .!= False
                                      <*> o .:? "disableBrowser"        .!= False
                                      <*> o .:? "compArguments"         .!= []
                                      <*> o .:? "arguments"             .!= []
                                      <*> o .:? "copyFiles"             .!= []

  parseJSON _ = mempty

-- testsuite description
data TestSuite =
  TestSuite { tsuiGroups           :: [(FilePath, String)]
            , tsuiRequiredPackages :: [Text]
            }

instance FromJSON TestSuite where
  parseJSON (Object o) = TestSuite <$> (groups =<< o .: "groups") <*> o .: "requiredPackages"
    where
      groups (Object o) = sequenceA $ map (\(k,v) -> (,) <$> pure (fromText k) <*> parseJSON v) (HM.toList o)
      groups _          = mempty
  parseJSON _          = mempty

testCaseLog :: TestOpts -> TestName -> Assertion -> Test
testCaseLog opts name assertion = testCase name assertion'
  where
    assertion'   = assertion `C.catch` \e@(HUnitFailure _ msg) -> do
      let errMsg = listToMaybe (filter (not . null) (lines (formatFailureReason msg)))
          err    = name ++ maybe "" (\x -> " (" ++ trunc (dropName x) ++ ")") errMsg
          trunc xs | length xs > 43 = take 40 xs ++ "..."
                   | otherwise = xs
          dropName xs | name `isPrefixOf` xs = drop (length name) xs
                      | otherwise            = xs
      modifyIORef (failedTests opts) (err:)
      C.throwIO e
{-
  run all files in path as stdio tests
  tests are:
   - .hs or .lhs files
   - that start with a lowercase letter
-}
allTestsIn :: MonadIO m => TestOpts -> FilePath -> FilePath -> m [Test]
allTestsIn testOpts testDir groupDir = shelly $ do
  cd testDir
  map (stdioTest testOpts) <$> findWhen (return . isTestFile) groupDir
  where
    testFirstChar c = isLower c || isDigit c
    isTestFile :: FilePath -> Bool
    isTestFile file =
      ("hs" `isExtensionOf` file || "lhs" `isExtensionOf` file) &&
      ((maybe False testFirstChar . listToMaybe . takeBaseName $ file) ||
      (takeBaseName file == "Main"))


outputLimit :: Int
outputLimit = 4096

truncLimit :: Int -> Text -> Text
truncLimit n t | T.length t >= n = T.take n t <> "\n[output truncated]"
               | otherwise       = t

instance Show StdioResult where
  show (StdioResult ex out err) =
    "\n>>> exit: " ++ show ex ++ "\n>>> stdout >>>\n" ++
    (T.unpack . T.strip) (truncLimit outputLimit out) ++
    "\n<<< stderr >>>\n" ++ (T.unpack . T.strip) (truncLimit outputLimit err) ++ "\n<<<\n"

stdioTest :: TestOpts -> FilePath -> Test
stdioTest testOpts file = testCaseLog testOpts file (stdioAssertion testOpts file)

stdioAssertion :: TestOpts -> FilePath -> Assertion
stdioAssertion testOpts file = do
  putStrLn ("running test: " ++ file)
  mexpected <- stdioExpected testOpts file
  case mexpected of
    Nothing -> putStrLn "test disabled"
    Just (expected, t) -> do
      actual <- runGhcjsResult testOpts file
      when (null actual) (putStrLn "warning: no test results")
      case t of
        Nothing -> return ()
        Just ms -> putStrLn ((padTo 35 file) ++ " - " ++ (padTo 35 "runhaskell") ++ " " ++ show ms ++ "ms")
      forM_ actual $ \((a,t),d) -> do
        assertEqual (file ++ ": " ++ d) expected a
        putStrLn ((padTo 35 file) ++ " - " ++ (padTo 35 d) ++ " " ++ show t ++ "ms")

padTo :: Int -> String -> String
padTo n xs | l < n     = xs ++ replicate (n-l) ' '
           | otherwise = xs
  where l = length xs

stdioExpected :: TestOpts -> FilePath -> IO (Maybe (StdioResult, Maybe Integer))
stdioExpected testOpts file = do
  settings <- settingsFor testOpts file
  if tsDisabled settings
    then return Nothing
    else do
      xs@[mex,mout,merr] <- mapM (readFilesIfExists.(map (replaceExtension (testsuiteLocation testOpts </> file))))
             [["exit"], ["stdout", "out"], ["stderr","err"]]
      if any isJust xs
        then return . Just $ (StdioResult (fromMaybe ExitSuccess $ readExitCode =<< mex)
                               (fromMaybe "" mout) (fromMaybe "" merr), Nothing)
        else do
          mr <- runhaskellResult testOpts settings file
          case mr of
            Nothing    -> assertFailure "cannot run `runhaskell'" >> return undefined
            Just (r,t) -> return (Just (r, Just t))

readFileIfExists :: FilePath -> IO (Maybe Text)
readFileIfExists file = do
  e <- doesFileExist file
  case e of
    False -> return Nothing
    True  -> Just <$> T.readFile file

readFilesIfExists :: [FilePath] -> IO (Maybe Text)
readFilesIfExists [] = return Nothing
readFilesIfExists (x:xs) = do
  r <- readFileIfExists x
  if (isJust r)
    then return r
    else readFilesIfExists xs

-- test settings
settingsFor :: TestOpts -> FilePath -> IO TestSettings
settingsFor opts file = do
  e <- doesFileExist (testsuiteLocation opts </> settingsFile)
  case e of
    False -> return def
    True -> do
      cfg <- B.readFile settingsFile'
      case Yaml.decodeEither cfg of
        Left err -> errDef
        Right t  -> return t
  where
    errDef = do
      putStrLn $ "error in test settings: " ++ settingsFile'
      putStrLn "running test with default settings"
      return def
    settingsFile  = replaceExtensions file "settings"
    settingsFile' = testsuiteLocation opts </> settingsFile

runhaskellResult :: TestOpts
                 -> TestSettings
                 -> FilePath
                 -> IO (Maybe (StdioResult, Integer))
runhaskellResult testOpts settings file = do
    let args = tsArguments settings
    r <- runProcess (testsuiteLocation testOpts </> takeDirectory file) (runhaskellProgram testOpts)
             (["-v0", "-w", takeFileName file] ++ args) ""
    return (fmap (\(s, i) -> (unmangleRunhaskellResult s, i)) r)

extraJsFiles :: FilePath -> IO [String]
extraJsFiles file =
  let jsFile = dropExtensions file <.> "foreign" <.> "js"
  in do
    e <- doesFileExist jsFile
    return $ if e then [jsFile] else []

runGhcjsResult :: TestOpts -> FilePath -> IO [((StdioResult, Integer), String)]
runGhcjsResult opts file = do
  settings <- settingsFor opts file
  if tsDisabled settings || (tsProf settings && noProfiling opts) || (tsDisableTravis settings && travisCI opts)
    then return []
    else do
      let unopt = if disableUnopt opts || tsDisableUnopt settings then [] else [False]
          opt   = if disableOpt opts || tsDisableOpt settings then [] else [True]
          runs  = unopt ++ opt
      concat <$> mapM (run settings) runs
    where
      run settings optimize = do
        output <- outputPath
        extraFiles <- extraJsFiles file
        cd <- getCurrentDirectory
        -- compile test
        let output'     = runDir opts </> output
            outputExe   = output' </> "a"
            outputExe'  = outputExe <.> "jsexe"
            outputUrl   = output </> "a.jsexe"
            outputBuild = output' </> "build"
            outputRun   = outputExe' </> ("all.js"::FilePath)
            outputClient = outputExe' </> ("client.js"::FilePath)
            outputHtml   = outputExe' </> ("wdindex.html"::FilePath)
            input  = file
            desc = ", optimization: " ++ show optimize
            opt = if optimize then ["-O2"] else []
            extraCompArgs = tsCompArguments settings
            prof = tsProf settings
            compileOpts = [ {-"-no-rts", "-no-stats"
                          , -} "-o", outputExe
                          , "-odir", outputBuild
                          , "-hidir", outputBuild
                          -- , "-use-base" , encodeString ((if prof then profBaseSymbs else baseSymbs) opts)
                          , takeFileName input
                          ] ++ opt ++ extraCompArgs ++ extraFiles
            args = tsArguments settings
            runTestPgm name disabled getPgm pgmArgs pgmArgs'
              | Just p <- getPgm opts, not (disabled settings) =
                  fmap (,name ++ desc) <$>
                      runProcess outputExe' p (pgmArgs ++ outputRun:pgmArgs' ++ args) ""
              | otherwise = return Nothing
            runBrowser session = fmap (,Client.sessionName session ++ desc) <$>
              Client.runTest outputUrl "wdindex.html" args session
        C.bracket (createDirectoryIfMissing False output')
                  (\_ -> removeDirectoryRecursive output') $ \_ -> do -- fixme this doesn't remove the output if the test program is stopped with ctrl-c
          createDirectoryIfMissing False outputBuild
          print compileOpts
          e <- liftIO $ runProcess (testsuiteLocation opts </> takeDirectory file) (ghcjsProgram opts) compileOpts ""
          case e of
            Nothing    -> assertFailure "cannot find ghcjs"
            Just (r,_) -> do
              when (stdioExit r /= ExitSuccess) (print r)
              assertEqual "compile error" ExitSuccess (stdioExit r)
          -- copy data files for test
          forM_ (tsCopyFiles settings) $ \cfile ->
            let cfile' = fromText (T.pack cfile)
            in  copyFile (testsuiteLocation opts </> takeDirectory file </> cfile') (outputExe' </> cfile')
          -- combine files with base bundle from incremental link
          [rts, out, lib] <- mapM (B.readFile . (\x -> outputExe' </> x))
                                ["rts.js", "out.js", "lib.js"]
          let runMain  = "\nh$main(h$mainZCZCMainzimain);\n"
              allNoRun = rts <> lib <> out -- (if prof then profBaseJs else baseJs) opts <> lib <> out
          B.writeFile outputRun (allNoRun <> runMain)
          B.writeFile outputClient (allNoRun <> wdRunMain opts)
          B.writeFile outputHtml (wdHtml opts)
          -- run with node.js and SpiderMonkey
          standaloneResults <- if tsBrowserOnly settings
            then return []
            else do
              nodeResult <- runTestPgm "node"           tsDisableNode           nodeProgram           ["--use_strict"] []
              smResult   <- runTestPgm "SpiderMonkey"   tsDisableSpiderMonkey   spiderMonkeyProgram   ["--strict"]     []
              jscResult  <- over (traverse . _1 . _1) unmangleJscResult <$>
                        runTestPgm "JavaScriptCore" tsDisableJavaScriptCore javaScriptCoreProgram []               ["--"]
              return (catMaybes [nodeResult, smResult, jscResult])
          -- run in browsers through Selenium WebDriver server
          browserResults <- if tsDisableBrowser settings
                            then return []
                            else catMaybes <$> mapM runBrowser (browsers opts)
          return $ standaloneResults ++ browserResults

-- jsc prefixes all sderr lines with "--> " and does not let us
-- return a nonzero exit status
unmangleJscResult :: StdioResult -> StdioResult
unmangleJscResult (StdioResult exit out err)
  | (x:xs) <- reverse (T.lines err)
  , Just code <- T.stripPrefix "--> GHCJS JSC exit status: " x
    = StdioResult (parseExit code) out (T.strip . T.unlines . reverse $ map unmangle xs)
  | otherwise = StdioResult exit out (T.strip . T.unlines . map unmangle . T.lines $ err)
  where
    unmangle xs = fromMaybe xs (T.stripPrefix "--> " xs)
    parseExit x = case reads (T.unpack x) of
                    [(0,"")] -> ExitSuccess
                    [(n,"")] -> ExitFailure n
                    _        -> ExitFailure 999

unmangleRunhaskellResult (StdioResult exit out err)
  = StdioResult exit
                out
                (T.unlines . filter f . T.lines $ err)
  where f t | "Loaded package environment from" `T.isPrefixOf` t = False
            | otherwise = True

outputPath :: IO FilePath
outputPath = do
  t <- (show :: Integer -> String) . round . (*1000) . utcTimeToPOSIXSeconds <$> getCurrentTime
  rnd <- show <$> randomRIO (1000000::Int,9999999)
  return . fromString $ "ghcjs_test_" ++ t ++ "_" ++ rnd

-- | returns Nothing if the program cannot be run
runProcess :: MonadIO m => FilePath -> FilePath -> [String] -> String -> m (Maybe (StdioResult, Integer))
runProcess workingDir pgm args input = do
  before <- liftIO getCurrentTime
  r <- liftIO (C.try $ timeout 180000000 (readProcessWithExitCode' workingDir pgm args input))
  case r of
    Left (e::C.SomeException) -> return Nothing
    Right Nothing -> return (Just (StdioResult ExitSuccess "" "process killed after timeout", 0))
    Right (Just (ex, out, err)) -> do
      after <- liftIO getCurrentTime
      return $
        case ex of -- fixme is this the right way to find out that a program does not exist?
          (ExitFailure 127) -> Nothing
          _                 ->
            Just ( StdioResult ex (T.pack out) (T.pack err)
                 , round $ 1000 * (after `diffUTCTime` before)
                 )

-- modified readProcessWithExitCode with working dir
readProcessWithExitCode'
    :: FilePath                 -- ^ Working directory
    -> FilePath                 -- ^ Filename of the executable (see 'proc' for details)
    -> [String]                 -- ^ any arguments
    -> String                   -- ^ standard input
    -> IO (ExitCode,String,String) -- ^ exitcode, stdout, stderr
readProcessWithExitCode' workingDir cmd args input = do
    let cp_opts = (proc cmd args) {
                    std_in  = CreatePipe,
                    std_out = CreatePipe,
                    std_err = CreatePipe,
                    cwd     = Just workingDir
                  }
    withCreateProcess cp_opts $
      \(Just inh) (Just outh) (Just errh) ph -> do

        out <- hGetContents outh
        err <- hGetContents errh

        -- fork off threads to start consuming stdout & stderr
        withForkWait  (C.evaluate $ rnf out) $ \waitOut ->
         withForkWait (C.evaluate $ rnf err) $ \waitErr -> do

          -- now write any input
          unless (null input) $
            ignoreSigPipe $ hPutStr inh input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose inh

          -- wait on the output
          waitOut
          waitErr

          hClose outh
          hClose errh

        -- wait on the process
        ex <- waitForProcess ph

        return (ex, out, err)

withCreateProcess
  :: CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess c action =
    C.bracketOnError (createProcess c) cleanupProcess
                     (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)

cleanupProcess :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
               -> IO ()
cleanupProcess (mb_stdin, mb_stdout, mb_stderr, ph) = do
    terminateProcess ph
    -- Note, it's important that other threads that might be reading/writing
    -- these handles also get killed off, since otherwise they might be holding
    -- the handle lock and prevent us from closing, leading to deadlock.
    maybe (return ()) (ignoreSigPipe . hClose) mb_stdin
    maybe (return ()) hClose mb_stdout
    maybe (return ()) hClose mb_stderr
    -- terminateProcess does not guarantee that it terminates the process.
    -- Indeed on Unix it's SIGTERM, which asks nicely but does not guarantee
    -- that it stops. If it doesn't stop, we don't want to hang, so we wait
    -- asynchronously using forkIO.
    _ <- forkIO (waitForProcess ph >> return ())
    return ()


withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either C.SomeException ()))
  C.mask $ \restore -> do
    tid <- forkIO $ C.try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either C.throwIO return
    restore (body wait) `C.onException` killThread tid

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = C.handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> C.throwIO e
-------------------

readExitCode :: Text -> Maybe ExitCode
readExitCode = fmap convert . readMaybe . T.unpack
  where
    convert 0 = ExitSuccess
    convert n = ExitFailure n

checkRequiredPackages :: FilePath -> [Text] -> IO ()
checkRequiredPackages ghcjsPkg requiredPackages = shelly . silently $ do
  installedPackages <- T.words <$> run "ghcjs-pkg" ["list", "--simple-output"]
  forM_ requiredPackages $ \pkg -> do
    when (not $ any ((pkg <> "-") `T.isPrefixOf`) installedPackages) $ do
      echo ("warning: package `" <> pkg <> "' is required by the test suite but is not installed")

prepareBaseBundle :: FilePath -> FilePath -> [Text] -> IO (B.ByteString, B.ByteString)
prepareBaseBundle testDir ghcjs extraArgs = shellyE . silently . sub . withTmpDir $ \tmp -> do
  cp (testDir </> "TestLinkBase.hs") tmp
  cp (testDir </> "TestLinkMain.hs") tmp
  cd tmp
  run_ ghcjs $ ["-generate-base", "TestLinkBase", "-o", "base", "TestLinkMain.hs"] ++ extraArgs
  cd "base.jsexe"
  xs <- mapM readBinary
    ["out.base.symbs", "out.base.js", "lib.base.js", "rts.js"]
  case xs of
    [symbs, js, lib, rts] -> return (symbs, rts <> lib <> js)
    _ -> error "prepareBaseBundle: unexpected result"

getEnvMay :: String -> IO (Maybe String)
getEnvMay xs = fmap Just (getEnv xs)
               `C.catch` \(_::C.SomeException) -> return Nothing

getEnvOpt :: MonadIO m => String -> m Bool
getEnvOpt xs = liftIO (maybe False ((`notElem` ["0","no"]).map toLower) <$> getEnvMay xs)

trim :: String -> String
trim = let f = dropWhile isSpace . reverse in f . f

shellyE :: Sh a -> IO a
shellyE m = do
  r <- newIORef (Left undefined)
  let wio r v = liftIO (writeIORef r v)
  a <- shelly $ (wio r . Right =<< m) `catch_sh` \(e::C.SomeException) -> wio r (Left e)
  readIORef r >>= \case
                     Left e  -> C.throw e
                     Right a -> return a

fromString :: String -> FilePath
fromString = id -- fromText . T.pack
