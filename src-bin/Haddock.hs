{-# Language PackageImports #-}

{-
  Haddock for GHCJS
 -}

module Main where

import           Control.Exception
import           Control.Monad.IO.Class
import           Data.Monoid
import           System.Exit

import           GHC
import           DynFlags
import           StaticFlags

import           Documentation.Haddock

import qualified Compiler.Info          as Info
import qualified Compiler.GhcjsProgram  as Ghcjs
import qualified Compiler.GhcjsPlatform as Ghcjs
import qualified Compiler.Settings      as Ghcjs

main :: IO ()
main = do
  args <- Info.getFullArguments -- adds wrapper arguments for Windows
  haddockWithGhc withGhcjs args

withGhcjs :: [Flag] -> Ghc a -> IO a
withGhcjs flags action = do
  libDir <- fmap snd (getGhcjsDirs flags)

  -- Catches all GHC source errors, then prints and re-throws them.
  let handleSrcErrors action' = flip handleSourceError action' $ \err -> do
        printException err
        liftIO exitFailure
      ghcFlags flags = [ option | Flag_OptGhc option <- flags ]
  withGhcjs' libDir (ghcFlags flags) (\_ -> handleSrcErrors action)

-- | Start a GHC session with the -haddock flag set. Also turn off
-- compilation and linking. Then run the given 'Ghc' action.
withGhcjs' :: String -> [String] -> (DynFlags -> Ghc a) -> IO a
withGhcjs' libDir flags ghcActs = runGhc (Just libDir) $ do
  dynflags  <- getSessionDynFlags
  dynflags' <- parseGhcFlags (gopt_set dynflags Opt_Haddock) {
    hscTarget = HscNothing,
    ghcMode   = CompManager,
    ghcLink   = NoLink
    }
  env <- liftIO Ghcjs.newGhcjsEnv
  let dynflags'' = Ghcjs.setGhcjsPlatform mempty env [] libDir $
                   updateWays $ addWay' (WayCustom "js") $
                   Ghcjs.setGhcjsSuffixes False $
                   gopt_unset dynflags' Opt_SplitObjs
  defaultCleanupHandler dynflags'' $ do
      -- ignore the following return-value, which is a list of packages
      -- that may need to be re-linked: Haddock doesn't do any
      -- dynamic or static linking at all!
      _ <- setSessionDynFlags dynflags''
      Ghcjs.fixNameCache
      ghcActs dynflags''
  where
    parseGhcFlags :: MonadIO m => DynFlags -> m DynFlags
    parseGhcFlags dynflags = do
      -- TODO: handle warnings?

      -- NOTA BENE: We _MUST_ discard any static flags here, because we cannot
      -- rely on Haddock to parse them, as it only parses the DynFlags. Yet if
      -- we pass any, Haddock will fail. Since StaticFlags are global to the
      -- GHC invocation, there's also no way to reparse/save them to set them
      -- again properly.
      --
      -- This is a bit of a hack until we get rid of the rest of the remaining
      -- StaticFlags. See GHC issue #8276.
      let flags' = discardStaticFlags flags
      (dynflags', rest, _) <- parseDynamicFlags dynflags (map noLoc $ flags')
      if not (null rest)
        then throw (HaddockException $ "Couldn't parse GHC options: " ++ unwords flags')
        else return dynflags'

getGhcjsDirs :: [Flag] -> IO (String, String)
getGhcjsDirs flags =
  case [ dir | Flag_GhcLibDir dir <- flags ] of
    [] -> error "haddock-ghcjs: missing -B option, cannot find library dir"
    xs -> return ("not available", last xs)

