-- | Compiler utility functions, mainly dealing with IO and files
module Compiler.Utils
    (
      -- * File utils
      touchFile
    , copyNoOverwrite
    , findFile
    , exeFileName
    , mkGhcjsSuf
    , mkGhcjsOutput
      -- * Source code and JS related utilities
    , ghcjsSrcSpan
    , partition_args_js
    , partition_args
      -- * User interaction
    , compilationProgressMsg
    ) where

import DynFlags
import DriverPhases
import GHC
import Platform
import SysTools
import SrcLoc
import Util          (looksLikeModuleName)

import Control.Monad
import Data.List         (isPrefixOf, partition)
import System.Directory  (doesFileExist, copyFile)
import System.FilePath

import Gen2.Utils

touchFile :: DynFlags -> FilePath -> IO ()
touchFile df file = do
--  putStrLn ("touchFile: " ++ file)
  e <- doesFileExist file
  when e (touch df "keep build system happy" file)

copyNoOverwrite :: FilePath -> FilePath -> IO ()
copyNoOverwrite from to = do
--  putStrLn ("copyNoOverWrite: " ++ from ++ " -> " ++ to)
  ef <- doesFileExist from
  et <- doesFileExist to
  when (ef && not et) (copyFile from to)

findFile :: (FilePath -> FilePath)      -- Maps a directory path to a file path
         -> [FilePath]                  -- Directories to look in
         -> IO (Maybe FilePath)         -- The first file path to match
findFile _            [] = return Nothing
findFile mk_file_path (dir : dirs)
  = do let file_path = mk_file_path dir
       b <- doesFileExist file_path
       if b then return (Just file_path)
            else findFile mk_file_path dirs

exeFileName :: DynFlags -> FilePath
exeFileName dflags
  | Just s <- outputFile dflags =
      -- unmunge the extension
      let s' = dropPrefix "js_" (drop 1 $ takeExtension s)
      in if null s'
           then dropExtension s <.> "jsexe"
           else dropExtension s <.> s'
  | otherwise =
      if platformOS (targetPlatform dflags) == OSMinGW32
           then "main.jsexe"
           else "a.jsexe"
  where
    dropPrefix prefix xs
      | prefix `isPrefixOf` xs = drop (length prefix) xs
      | otherwise              = xs


partition_args_js :: [Located String] -> ( [(String, Maybe Phase)]  
                                           -- ^ Haskell source files
                                         , [(String, Maybe Phase)]
                                           -- ^ Non-haskell source files
                                         , [FilePath]
                                           -- ^ JavaScript objects files
                                         , [FilePath]
                                           -- ^ Non-JS object files
                                         )
partition_args_js fileish_args = (hs_srcs, non_hs_srcs, js_objs, objs)
  where
    normal_fileish_paths    = map (normalise . unLoc) fileish_args
    (srcs, objs0)           = partition_args normal_fileish_paths [] []
    (js_objs, objs)         = partition isJsFile objs0
    (hs_srcs, non_hs_srcs)  = partition haskellish srcs


-- -----------------------------------------------------------------------------
-- Splitting arguments into source files and object files.  This is where we
-- interpret the -x <suffix> option, and attach a (Maybe Phase) to each source
-- file indicating the phase specified by the -x option in force, if any.

partition_args :: [String] -> [(String, Maybe Phase)] -> [String]
               -> ([(String, Maybe Phase)], [String])
partition_args [] srcs objs = (reverse srcs, reverse objs)
partition_args ("-x":suff:args) srcs objs
  | "none" <- suff      = partition_args args srcs objs
  | StopLn <- phase     = partition_args args srcs (slurp ++ objs)
  | otherwise           = partition_args rest (these_srcs ++ srcs) objs
        where phase = startPhase suff
              (slurp,rest) = break (== "-x") args
              these_srcs = zip slurp (repeat (Just phase))
partition_args (arg:args) srcs objs
  | looks_like_an_input arg = partition_args args ((arg,Nothing):srcs) objs
  | otherwise               = partition_args args srcs (arg:objs)

    {-
      We split out the object files (.o, .dll) and add them
      to ldInputs for use by the linker.

      The following things should be considered compilation manager inputs:

       - haskell source files (strings ending in .hs, .lhs or other
         haskellish extension),

       - module names (not forgetting hierarchical module names),

       - things beginning with '-' are flags that were not recognised by
         the flag parser, and we want them to generate errors later in
         checkOptions, so we class them as source files (#5921)

       - and finally we consider everything not containing a '.' to be
         a comp manager input, as shorthand for a .hs or .lhs filename.

      Everything else is considered to be a linker object, and passed
      straight through to the linker.
    -}
looks_like_an_input :: String -> Bool
looks_like_an_input m =  isSourceFilename m
                      || looksLikeModuleName m
                      || "-" `isPrefixOf` m
                      || '.' `notElem` m



haskellish :: (String, Maybe Phase) -> Bool
haskellish (f,Nothing) =
  looksLikeModuleName f || isHaskellUserSrcFilename f || '.' `notElem` f
haskellish (_,Just phase) =
  phase `notElem` [As, Cc, Cobjc, Cobjcpp, CmmCpp, Cmm, StopLn]


isJsFile :: FilePath -> Bool
isJsFile = (==".js") . takeExtension

mkGhcjsOutput :: String -> String
mkGhcjsOutput "" = ""
mkGhcjsOutput file
  | ext == ".hi"     = replaceExtension file ".js_hi"
  | ext == ".o"      = replaceExtension file ".js_o"
  | ext == ".dyn_hi" = replaceExtension file ".js_dyn_hi"
  | ext == ".dyn_o"  = replaceExtension file ".js_dyn_o"
  | otherwise        = replaceExtension file (".js_" ++ drop 1 ext)
  where
    ext = takeExtension file
mkGhcjsSuf :: String -> String
mkGhcjsSuf "o"      = "js_o"
mkGhcjsSuf "hi"     = "js_hi"
mkGhcjsSuf "dyn_o"  = "js_dyn_o"
mkGhcjsSuf "dyn_hi" = "js_dyn_hi"
mkGhcjsSuf xs       = "js_" ++ xs -- is this correct?

