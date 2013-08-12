{-# LANGUAGE DefaultSignatures,
             OverloadedStrings,
             TupleSections #-}
{-
  GHCJS linker, collects dependencies from
    the object files (.js_o), which contain linkable 
    units with dependency information
-}
module Gen2.Linker where

import           Control.Applicative
import           Control.Monad

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Char                (isDigit)
import qualified Data.Foldable            as F
import           Data.List                (partition, isSuffixOf, isPrefixOf)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromMaybe)
import           Data.Monoid
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import qualified Data.Vector              as V


import           Language.Javascript.JMacro
import           System.FilePath          (dropExtension, splitPath, (<.>), (</>))
import           System.Directory         (createDirectoryIfMissing, doesFileExist)
import           Text.PrettyPrint.Leijen.Text (displayT, renderPretty)

import           Config
import           Module                   (ModuleName, moduleNameString)

import           Compiler.Info
import           Gen2.StgAst
import           Gen2.Rts                 (rtsStr)
import           Gen2.Shim
import           Gen2.Printer             (pretty)
import qualified Gen2.Compactor           as Compactor
import           Gen2.ClosureInfo         hiding (Fun)
import           Gen2.Object

link :: Bool
     -> String       -- ^ output file/directory
     -> [FilePath]   -- ^ directories to load package modules, end with package name
     -> [FilePath]   -- ^ the object files we're linking
     -> [ModuleName] -- ^ modules to use as roots (include all their functions and deps)
     -> IO [String]  -- ^ arguments for the closure compiler to minify our result
link debug out searchPath objFiles pageModules = do
  let (objFiles', extraFiles) = partition (".js_o" `isSuffixOf`) objFiles
  metas <- mapM readDepsFile objFiles'
  let roots = filter ((`elem` mods) . depsModule) metas
  T.putStrLn ("linking " <> T.pack out <> ": " <> T.intercalate ", " (map depsModule roots))
--  print searchPath
--  print objFiles
--  print pageModules
  (allDeps, src, infos) <- collectDeps (lookup metas) (S.union rtsDeps (S.fromList $ concatMap modFuns roots))
  createDirectoryIfMissing False out
  BL.writeFile (out </> "out.js") (renderLinker debug src infos)
  writeFile (out </> "rts.js") rtsStr
  getShims extraFiles allDeps (out </> "lib.js", out </> "lib1.js")
  writeHtml out
  combineFiles out
  return []
  where
    mods = map (T.pack . moduleNameString) pageModules
    pkgLookup       = M.fromList (map (\p -> (pkgFromPath p, p)) searchPath)
    -- pkg name-version is last path element, except when there's a ghc-version path element after
    pkgFromPath path | (a:b:_) <- reverse (splitPath' path) =
      if (("ghc-"++cProjectVersion) `isPrefixOf` a) then T.pack b else T.pack a
    pkgFromPath _ = mempty
    pkgLookupNoVer  = M.mapKeys dropVersion pkgLookup
    localLookup metas  =
      M.fromList $ zipWith (\m o -> (depsModule m, dropExtension o)) metas objFiles

    -- | lookup fun in known packages first, then object files specified on command
    --   line, otherwise files in the current dir
    lookup metas ext fun
      | Just path <- M.lookup (funPkgTxt      fun) pkgLookup = return (path </> modPath)
      | Just path <- M.lookup (funPkgTxtNoVer fun) pkgLookupNoVer = return (path </> modPath)
      | otherwise = return $ maybe ("." </> modPath) (<.> ext)
                             (M.lookup (funModule fun) (localLookup metas))
      where
        modPath = (T.unpack $ T.replace "." "/" (funModule fun)) <.> ext

renderLinker :: Bool -> JStat -> [ClosureInfo] -> BL.ByteString
renderLinker debug stat infos 
   = TLE.encodeUtf8 . displayT . renderPretty 0.8 150 . pretty $
       Compactor.compact debug stat infos

splitPath' :: FilePath -> [FilePath]
splitPath' = map (filter (`notElem` "/\\")) . splitPath

-- fixme get dependencies from DynFlags instead
getShims :: [FilePath] -> Set Fun -> (FilePath, FilePath) -> IO ()
getShims extraFiles deps (fileBefore, fileAfter) = do
  base <- (</> "shims") <$> getGlobalPackageBase
  ((before, beforeFiles), (after, afterFiles)) <- collectShims base pkgDeps
  T.writeFile fileBefore before
  writeFile (fileBefore <.> "files") (unlines beforeFiles)
  t' <- mapM T.readFile extraFiles
  T.writeFile fileAfter (T.unlines $ after : t')
  writeFile (fileAfter <.> "files") (unlines $ afterFiles ++ extraFiles)
    where
      pkgDeps = map (\(Package n v) -> (n, fromMaybe [] $ parseVersion v))
                  (S.toList $ S.map funPackage deps)

-- convenience: combine lib.js, rts.js, lib1.js, out.js to all.js that can be run
-- directly with node or spidermonkey
combineFiles :: FilePath -> IO ()
combineFiles fp = do
  files <- mapM (T.readFile.(fp</>)) ["lib.js", "rts.js", "lib1.js", "out.js"]
  T.writeFile (fp</>"all.js") (mconcat (files ++ [runMain]))

runMain :: Text
runMain = "\nh$main(h$mainZCMainzimain);\n"

basicHtml :: Text
basicHtml = "<!DOCTYPE html>\n\
            \<html>\n\
            \  <head>\n\
            \    <script language=\"javascript\" src=\"lib.js\"></script>\n\
            \    <script language=\"javascript\" src=\"rts.js\"></script>\n\
            \    <script language=\"javascript\" src=\"lib1.js\"></script>\n\
            \    <script language=\"javascript\" src=\"out.js\"></script>\n\
            \  </head>\n\
            \  <body>\n\
            \  </body>\n\
            \  <script language=\"javascript\">\n\
            \    " <> runMain <> "\n\
            \  </script>\n\
            \</html>\n"

writeHtml :: FilePath -> IO ()
writeHtml out = do
  e <- doesFileExist htmlFile
  when (not e) (T.writeFile htmlFile basicHtml)
  where
    htmlFile = out </> "index" <.> "html"

-- drop the version from a package name
-- fixme this is probably a bit wrong but should only be necessary
-- for wired-in packages base, ghc-prim, integer-gmp, main, rts
dropVersion :: Text -> Text
dropVersion = fst . splitVersion

splitVersion :: Text -> (Text, Text)
splitVersion t
  | T.null ver || T.any (`notElem` "1234567890.") ver
      = (t, mempty)
  | T.null name = (mempty, mempty)
  | otherwise   = (T.reverse (T.tail name), T.reverse ver)
  where
    (ver, name) = T.break (=='-') (T.reverse t)

-- | get all functions in a module
modFuns :: Deps -> [Fun]
modFuns (Deps p m d) = map fst (M.toList d)

-- | get all dependencies for a given set of roots
getDeps :: (String -> Fun -> IO FilePath) -> Set Fun -> IO (Set Fun)
getDeps lookup fun = go S.empty M.empty (S.toList fun)
  where
    go :: Set Fun -> Map (Package,Text) Deps -> [Fun] -> IO (Set Fun)
    go result _    []         = return result
    go result deps ffs@(f:fs) =
      let key = (funPackage f, funModule f)
      in  case M.lookup key deps of
            Nothing -> lookup "js_o" f >>= readDepsFile >>=
                           \d -> go result (M.insert key d deps) ffs
            Just (Deps _ _ d)  -> let ds = filter (`S.notMember` result)
                                           (maybe [] S.toList $ M.lookup f d)
                              in  go (S.insert f result) deps (ds++fs)

-- | get all modules used by the roots and deps
getDepsSources :: (String -> Fun -> IO FilePath) -> Set Fun -> IO (Set Fun, [(FilePath, Set Fun)])
getDepsSources lookup funs = do
  allDeps <- getDeps lookup funs
  allPaths <- mapM (\x -> (,S.singleton x) <$> lookup "js_o" x) (S.toList allDeps)
  return $ (allDeps, M.toList (M.fromListWith S.union allPaths))

-- | collect source snippets
collectDeps :: (String -> Fun -> IO FilePath) -> Set Fun -> IO (Set Fun, JStat, [ClosureInfo])
collectDeps lookup roots = do
  (allDeps, srcs) <- getDepsSources lookup roots
  (stats, infos) <- unzip <$> mapM (uncurry extractDeps) srcs
  return (allDeps, mconcat stats, concat infos)

extractDeps :: FilePath -> Set Fun -> IO (JStat, [ClosureInfo])
extractDeps file funs = do
  let symbs = S.fromList $ map funSymbol (S.toList funs)
  l <- readObjectFileKeys (any (`S.member` symbs)) file
  return (mconcat (map oiStat l), concatMap oiClInfo l)

-- Fun -> packagename-packagever
funPkgTxt :: Fun -> Text
funPkgTxt f = packageName pkg <> "-" <> packageVersion pkg
   where
    pkg = funPackage f

-- Fun -> packagename
funPkgTxtNoVer :: Fun -> Text
funPkgTxtNoVer = packageName . funPackage

-- dependencies for the RTS, these need to be always linked
rtsDeps :: Set Fun
rtsDeps =
 let mkDep (p,m,s) = Fun (Package p "") m s
 in S.fromList $ map mkDep
     [ ("base",       "GHC.Conc.Sync",          "h$baseZCGHCziConcziSynczireportError")
     , ("base",       "Control.Exception.Base", "h$baseZCControlziExceptionziBasezinonTermination" )
     , ("ghcjs-prim", "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimziJSRef")
     ]

