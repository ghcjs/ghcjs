{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO(hFlush, stdout)
import Network.HTTP
import Network.Browser
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Data.List (nub, isPrefixOf)
import Data.String (fromString)
import Control.Applicative ((<$>))
import Control.Monad (forM_, forM, when, unless)
import Shelly
       (lastExitCode, mkdir_p, cp, errExit, writefile, print_stdout, run,
        test_f, fromText, toTextIgnore, setStdin, readfile, pwd, run_,
        withTmpDir, cd, mkdir, ls, echo, shelly)
import qualified Data.Yaml as Yaml
import Control.Monad.IO.Class (MonadIO(..))
import Filesystem (getHomeDirectory)
import Filesystem.Path
       (directory, (<.>), basename, extension, dropExtension, (</>))
import Filesystem.Path.CurrentOS (encodeString, toText)
import qualified Data.Text as T
       (toLower, takeWhile, unlines, isPrefixOf, null, pack, intercalate,
        words, lines, unpack)
import Gen2.Shim
       (VersionRange(..), Shim(..), foldShim, inRange, readShim,
        parseVersion, collectShim, versionRangeToBuildDep)

main = shelly $ do
    let userName = "HamishMackenzie" -- TODO stop hard coding this
    liftIO $ putStr "Password: "
    liftIO $ hFlush stdout
    password <- liftIO getLine
    home <- liftIO getHomeDirectory
    let ghcjs = home </> ".ghcjs" </> "x86_64-darwin-0.1.0-7.8.3" </> "ghcjs" -- TODO stop hard coding this
        boot = ghcjs </> "ghcjs-boot"
        patchesDir = boot </> "patches"
        shims = ghcjs </> "shims"

    -- Get a list of all the hackage.haskell.org packages
    allPackages <- T.lines <$> (print_stdout False $ run "cabal" ["list", "--simple-output"])

    -- For every package and its version number
    forM_ [(pkg, parseVersion ver) | [pkg, ver] <- map T.words allPackages] $ \case
        (pkg, Just ver) | pkg /= "rts" -> do
            -- Read the yaml file from the shims repo
            mbShim <- liftIO $ readShim (encodeString shims) (pkg, ver)
            case mbShim of
                Just shim -> withTmpDir $ \tmpDir -> do
                    -- Check if this package is in the version range specified in the yaml file
                    let isInRange = inRange ver shim

                    -- Something like "package-1.0.0.0"
                    let packageAndVer = fromText $ pkg <> "-" <> T.intercalate "." (map (T.pack . show) ver)

                    -- Unpack the package
                    cd tmpDir
                    run_ "cabal" ["unpack", toTextIgnore packageAndVer]
                    cd packageAndVer

                    -- Look for a patch file
                    versionedPatchExists <- test_f $ patchesDir </> packageAndVer <.> "patch"
                    let patchFile = patchesDir </> (if versionedPatchExists
                                                        then packageAndVer
                                                        else fromText pkg) <.> "patch"

                    -- If we have a patch file use it
                    patchExists <- test_f patchFile
                    when (patchExists && isInRange) $ do
                            readfile patchFile >>= setStdin
                            run_ "patch" ["-p1", "-N"]

                    -- Work out what to add to the cabal file
                    extra <- if isInRange
                                then forM (foldShim pkg ver shim) $ \f -> do
                                    -- This package is one ghcjs supports so add the js-sources
                                    let file = fromString f
                                    -- Copy js-soruce into place
                                    mkdir_p (directory $ file)
                                    cp (shims </> file) file
                                    return ("js-sources: " <>  T.pack f)
                                else return [
                                    -- Not supported by ghcjs add a build dep to exclude it
                                    "if impl(ghcjs)",
                                    "  build-depends: " <> pkg <> " "
                                        <> versionRangeToBuildDep (versionRange shim)]

                    -- Update the cabal file (find the line that starts with "library" and
                    -- add the extra stuff just after that (indented to match)
                    cabalFile <- readfile $ fromText pkg <.> "cabal"
                    writefile (fromText pkg <.> "cabal") . T.unlines $
                        case span (not . isLibraryLine) $ T.lines cabalFile of
                            (beforeLib, libLine:afterLib) ->
                                let indentAmount =
                                        case filter (not . T.null) afterLib of
                                            nonBlank:_ -> T.takeWhile isSpace nonBlank
                                            _ -> "  "
                                in beforeLib <> [libLine] <> map (indentAmount <>) extra <> afterLib
                            (beforeLib, []) -> beforeLib -- Probably an old .cabal file

                    -- Try to make a new tat.gz file.  If we are not supporting this version any
                    -- way then ignore errors
                    errExit isInRange $ run_ "cabal" ["sdist"]
                    e <- lastExitCode
                    when (e == 0) $ do
                        -- Upload the package definition
                        run_ "curl" [
                            "-X", "PUT",
                            "-H", "Content-Type: text/plain",
                            "--data-binary", "@" <> pkg <> ".cabal",
                            "http://" <> userName <> ":" <> T.pack password
                                <> "@hackage.ghcjs.org/package/"
                                <> toTextIgnore packageAndVer <> "/"
                                <> pkg <> ".cabal"]
                        -- Upload the tar.gz file
                        run_ "curl" [
                            "-X", "PUT",
                            "-H", "Content-Type: application/x-tar",
                            "-H", "Content-Encoding: gzip",
                            "--data-binary", "@" <> toTextIgnore ("dist" </> packageAndVer <.> "tar.gz"),
                            "http://" <> userName <> ":" <> T.pack password
                                <> "@hackage.ghcjs.org/package/"
                                <> toTextIgnore packageAndVer <> "/"
                                <> toTextIgnore packageAndVer <> ".tar.gz"]
                Nothing   -> return () -- No yaml file in shims
        _ -> return () -- rts package (which we are skipping) or invalid version
  where
    isLibraryLine = isLibraryLine' . T.toLower
    isLibraryLine' l = l == "library" || "library " `T.isPrefixOf` l
