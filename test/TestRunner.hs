{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char (isLower, isDigit)
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Filesystem (removeTree, isFile)
import           Filesystem.Path (replaceExtension, basename, directory, extension, addExtension)
import           Filesystem.Path.CurrentOS (encodeString, decodeString)
import           Prelude hiding (FilePath)
import           Shelly
import           System.Exit (ExitCode(..), exitFailure)
import           System.Process (readProcessWithExitCode)
import           System.Random (randomRIO)
import           Test.Framework
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit.Base (assertBool, assertFailure, assertEqual, Assertion)
import           Text.Read (readMaybe)

main = defaultMain =<< tests

-- fail if any of these are not installed
requiredPackages :: [TL.Text]
requiredPackages = [ "ghc-prim"
                   , "integer-gmp"
                   , "base"
                   , "containers"
                   , "array"
                   , "deepseq"
                   , "template-haskell"
                   , "random"
                   , "syb"
                   , "transformers"
                   ]

tests = do
  checkRequiredPackages
  fay     <- allTestsIn "test/fay"
  ghc     <- allTestsIn "test/ghc"
  arith   <- allTestsIn "test/arith"
  integer <- allTestsIn "test/integer"
  return [ testGroup "Tests from the Fay testsuite" fay
         , testGroup "Tests from the GHC testsuite" ghc
         , testGroup "Arithmetic" arith
         , testGroup "Integer" integer
         ]

{-
  run all files in path as stdio tests
  tests are:
   - .hs or .lhs files
   - that start with a lowercase letter
-}
-- allTestsIn :: FilePath -> IO [Test]
allTestsIn path = shelly $
  map stdioTest <$> findWhen (return . isTestFile) path
  where
    testFirstChar c = isLower c || isDigit c
    isTestFile file =
      (extension file == Just "hs" || extension file == Just "lhs") &&
      (maybe False testFirstChar . listToMaybe . encodeString . basename $ file)

{-
  a stdio test tests two things:
  stdout/stderr/exit output must be either:
     - the same as filename.out/filename.err/filename.exit (if any exists)
     - the same as runhaskell output (otherwise)
  the javascript is run with `js' (SpiderMonkey) and `node` (v8)
  if they're in $PATH.
-}
data StdioResult = StdioResult { stdioExit :: ExitCode
                               , stdioOut :: Text
                               , stdioErr :: Text
                               } -- deriving (Show)
instance Eq StdioResult where
  (StdioResult e1 ou1 er1) == (StdioResult e2 ou2 er2) =
    e1 == e2 && (T.strip ou1 == T.strip ou2) && (T.strip er1 == T.strip er2)

instance Show StdioResult where
  show (StdioResult ex out err) = 
    "\n>>> exit: " ++ show ex ++ "\n>>> stdout >>>\n" ++ 
    T.unpack out ++ "\n<<< stderr >>>\n" ++ T.unpack err ++ "\n<<<\n"

stdioTest :: FilePath -> Test
stdioTest file = testCase (encodeString file) (stdioAssertion file)

stdioAssertion :: FilePath -> Assertion
stdioAssertion file = do
  putStrLn ("running test: " ++ encodeString file)
  expected <- stdioExpected file
  actual <- runGhcjsResult file
  assertBool "no test results, install node and/or SpiderMonkey" (not $ null actual)
  forM_ actual $ \(a,d) -> assertEqual (encodeString file ++ ": " ++ d) expected a

stdioExpected :: FilePath -> IO StdioResult
stdioExpected file = do
  xs@[mex,mou,mer] <- mapM (readFileIfExists.(replaceExtension file)) ["exit", "out", "err"]
  if any isJust xs
    then return $ StdioResult (fromMaybe ExitSuccess $ readExitCode =<< mex)
                     (fromMaybe "" mou) (fromMaybe "" mer)
    else do
      mr <- runhaskellResult file
      case mr of
        Nothing -> assertFailure "cannot run `runhaskell'" >> return undefined
        Just r  -> return r

readFileIfExists :: FilePath -> IO (Maybe Text)
readFileIfExists file = do
  e <- isFile file
  case e of
    False -> return Nothing
    True  -> Just <$> T.readFile (encodeString file)

runhaskellResult :: FilePath -> IO (Maybe StdioResult)
runhaskellResult file =
  runProcess "runhaskell" [includeOpt file, encodeString file] ""

includeOpt :: FilePath -> String
includeOpt fp = "-i" <> encodeString (directory fp)

extraJsFiles :: FilePath -> IO [String]
extraJsFiles file =
  let jsFile = replaceExtension file "js"
  in do
    e <- isFile jsFile
    return $ if e then [encodeString jsFile] else []

-- | gen2 only so far
runGhcjsResult :: FilePath -> IO [(StdioResult, String)]
runGhcjsResult file = concat <$> mapM run [False, True]
  where
    run optimize = do
      output <- outputPath
      extra <- extraJsFiles file
      let outputG2 = addExtension output "gen2.jsexe"
          outputRun = encodeString $ outputG2 </> ("all.js"::FilePath)
          input  = encodeString file
          desc = ", optimization: " ++ show optimize
          inc = includeOpt file
          compileOpts = if optimize
                          then [inc, "-o", encodeString output, "-O2"] ++ [input] ++ extra
                          else [inc, "-o", encodeString output] ++ [input] ++ extra
      e <- liftIO $ runProcess "ghcjs" compileOpts ""
      case e of
        Nothing -> assertFailure "cannot find ghcjs"
        Just r  -> assertEqual "compile error" ExitSuccess (stdioExit r)
      nodeResult <- fmap (,"node" ++ desc) <$> runProcess "node" [outputRun] ""
      smResult   <- fmap (,"SpiderMonkey" ++ desc) <$> runProcess "js" [outputRun] ""
      liftIO $ removeTree outputG2
      return $ catMaybes [nodeResult, smResult]


outputPath :: IO FilePath
outputPath = do
  t <- show . round . (*1000) . utcTimeToPOSIXSeconds <$> getCurrentTime
  rnd <- show <$> randomRIO (1000000::Int,9999999)
  return . decodeString $ "ghcjs_test_" ++ t ++ "_" ++ rnd

-- | returns Nothing if the program cannot be run
runProcess :: MonadIO m => FilePath -> [String] -> String -> m (Maybe StdioResult)
runProcess pgm args input = do
--  liftIO $ (putStrLn $ "running: "  ++ (encodeString pgm) ++ " " ++ show args)
  (ex, out, err) <- liftIO $ readProcessWithExitCode (encodeString pgm) args input
  return $ case ex of -- fixme is this the right way to find out that a program does not exist?
    (ExitFailure 127) -> Nothing
    _                 -> Just $ StdioResult ex (T.pack out) (T.pack err)


{-
  a mocha test changes to the directory,
  runs the action, then runs `mocha'
  fails if mocha exits nonzero
 -}
mochaTest :: FilePath -> IO a -> IO b -> Test
mochaTest dir pre post = do
  undefined

writeFileT :: FilePath -> Text -> IO ()
writeFileT fp t = T.writeFile (encodeString fp) t

readFileT :: FilePath -> IO Text
readFileT fp = T.readFile (encodeString fp)

readExitCode :: Text -> Maybe ExitCode
readExitCode = fmap convert . readMaybe . T.unpack
  where
    convert 0 = ExitSuccess
    convert n = ExitFailure n

checkRequiredPackages :: IO ()
checkRequiredPackages = shelly . silently $ do
  installedPackages <- TL.words <$> run "ghcjs-pkg" ["list", "--simple-output"]
  forM_ requiredPackages $ \pkg -> do
    when (not $ any ((pkg <> "-") `TL.isPrefixOf`) installedPackages) $ do
      echo ("package `" <> pkg <> "' is required by the test suite but is not installed")
      liftIO exitFailure
