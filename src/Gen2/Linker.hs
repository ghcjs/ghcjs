{-# LANGUAGE DefaultSignatures,
             OverloadedStrings,
             TupleSections,
             LambdaCase  #-}
{-
  GHCJS linker, collects dependencies from
    the object files (.js_o), which contain linkable 
    units with dependency information
-}
module Gen2.Linker where

import           Control.Applicative
import           Control.Lens hiding ((<.>))
import           Control.Monad
import           Control.Concurrent.MVar

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Char                (isDigit)
import qualified Data.Foldable            as F
import           Data.List                ( partition, isSuffixOf, isPrefixOf
                                          , intercalate, group, sort)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromMaybe, listToMaybe)
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

import           DynFlags
import           Config
import           Module                   (PackageId, packageIdString, ModuleName, moduleNameString)

import           Compiler.Info
import           Gen2.StgAst
import           Gen2.Rts                 (rtsStr)
import           Gen2.Shim
import           Gen2.Printer             (pretty)
import qualified Gen2.Compactor           as Compactor
import           Gen2.ClosureInfo         hiding (Fun)
import           Gen2.Object
import           Gen2.Utils
import           Gen2.RtsTypes

link :: DynFlags
     -> Bool
     -> FilePath                  -- ^ output file/directory
     -> [FilePath]                -- ^ include path for home package
     -> [(PackageId, [FilePath])] -- ^ directories to load package modules
     -> [FilePath]                -- ^ the object files we're linking
     -> [FilePath]                -- ^ extra js files to include
     -> (Fun -> Bool)             -- ^ functions from the objects to use as roots (include all their deps)
     -> IO [String]               -- ^ arguments for the closure compiler to minify our result
link dflags debug out include pkgs objFiles jsFiles isRootFun = do
  objDeps <- mapM readDepsFile objFiles
  let roots = S.fromList . filter isRootFun $
        concatMap (M.keys . depsDeps) objDeps
      rootMods = map (T.unpack . head) . group . sort . map funModule . S.toList $ roots
  -- putStrLn ("objects: " ++ show (traverse . _1 %~ packageIdString $ pkgs))
  compilationProgressMsg dflags $
    "Linking " ++ out ++ " (" ++ intercalate "," rootMods ++ ")"
  c <- newMVar M.empty
  (allDeps, src, infos) <-
    collectDeps (lookupFun c $ zip objDeps objFiles) 
                (roots `S.union` rtsDeps (map fst pkgs))
  createDirectoryIfMissing False out
  BL.writeFile (out </> "out.js") (renderLinker debug src infos)
  writeFile (out </> "rts.js") rtsStr
  getShims jsFiles (map fst pkgs) (out </> "lib.js", out </> "lib1.js")
  writeHtml out
  combineFiles out
  return []
  where
    pkgPaths :: Map Text [FilePath]
    pkgPaths = M.fromList pkgs'
      where
        pkgs' = concatMap (\(pkg, dirs) -> map (,dirs) (packageIdToStrings pkg)) pkgs

    packageIdToStrings :: PackageId -> [Text]
    packageIdToStrings pkg
      | isWiredInPackage xs = [ T.pack xs, dropVersion . T.pack $ xs ]
      | otherwise           = [ T.pack xs ]
      where
        xs = packageIdString pkg

    lookupFun :: MVar (Map (Package, Text, String) FilePath)
              -> [(Deps,FilePath)]
              -> String -> Fun -> IO FilePath
    lookupFun cache objs = lCached
      where
        lCached :: String -> Fun -> IO FilePath
        lCached ext fun = do
          c <- takeMVar cache
          let k = (funPackage fun, funModule fun, ext)
          case M.lookup k c of
            Just p -> putMVar cache c >> return p
            Nothing -> do
              p <- l ext fun
              -- putStrLn ("looked up: " ++ p)
              putMVar cache (M.insert k p c)
              return p
        objs' = M.fromList $ map (\(Deps pkg m _, p) -> ((pkg,m),p)) objs
        l ext fun
           -- already loaded objects
           | Just p   <- M.lookup (funPackage fun, funModule fun) objs' = return p
           -- known package in dependencies
           | Just pkg <- M.lookup (funPkgTxt fun) pkgPaths = searchPaths [] pkg
           -- known wired-in package (no version)
           | Just pkg <- M.lookup (funPkgTxtNoVer fun) pkgPaths = searchPaths [] pkg
           -- search in include dirs
           | otherwise = searchPaths [] include
           where
             modPath = (T.unpack $ T.replace "." "/" (funModule fun)) <.> ext
             searchPaths searched [] = error $ "cannot find symbol: " ++ showFun fun
                                            ++ "\nsearched in:\n" ++ unlines searched
             searchPaths searched (x:xs) =
                let p = x </> modPath
                in  doesFileExist p >>=
                      \case
                         False -> searchPaths (p:searched) xs
                         True  -> return p

renderLinker :: Bool -> JStat -> [ClosureInfo] -> BL.ByteString
renderLinker debug stat infos
   = TLE.encodeUtf8 . displayT . renderPretty 0.8 150 . pretty $
       Compactor.compact debug stat infos

splitPath' :: FilePath -> [FilePath]
splitPath' = map (filter (`notElem` "/\\")) . splitPath

-- fixme the wired-in package id's we get from GHC we have no version
getShims :: [FilePath] -> [PackageId] -> (FilePath, FilePath) -> IO ()
getShims extraFiles deps (fileBefore, fileAfter) = do
  base <- (</> "shims") <$> getGlobalPackageBase
  ((before, beforeFiles), (after, afterFiles))
     <- collectShims base (map convertPkg deps)
  T.writeFile fileBefore before
  writeFile (fileBefore <.> "files") (unlines beforeFiles)
  t' <- mapM T.readFile extraFiles
  T.writeFile fileAfter (T.unlines $ after : t')
  writeFile (fileAfter <.> "files") (unlines $ afterFiles ++ extraFiles)
    where
      convertPkg p =
        let (n,v) = splitVersion . T.pack . packageIdString $ p
        in  (n, fromMaybe [] $ parseVersion v)

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
dropVersion :: Text -> Text
dropVersion = fst . splitVersion

splitVersion :: Text -> (Text, Text)
splitVersion t
  | T.null ver || T.null name  = (t, mempty)
  | not (validVer ver) =
      let vn@(ver', name') = T.break (=='-') name
      in if validVer ver' && not (T.null name)
           then (T.reverse (T.tail name'), T.reverse ver')
           else (t, mempty)
  | otherwise   = (T.reverse (T.tail name), T.reverse ver)
      where
        validVer v = T.all (`elem` "1234567890.") v && not (T.null v)
        (ver, name) = T.break (=='-') (T.reverse t)

-- | get all functions in a module
modFuns :: Deps -> [Fun]
modFuns (Deps p m d) = map fst (M.toList d)

-- | get all dependencies for a given set of roots
getDeps :: (String -> Fun -> IO FilePath) -> Set Fun -> IO (Set Fun)
getDeps lookup fun = go S.empty M.empty (S.toList fun)
  where
    go :: Set Fun -> Map (Package, Text) Deps -> [Fun] -> IO (Set Fun)
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
getDepsSources :: (String -> Fun -> IO FilePath)
               -> Set Fun
               -> IO (Set Fun, [(FilePath, Set Fun)])
getDepsSources lookup roots = do
  allDeps <- getDeps lookup roots
  allPaths <- mapM (\x -> (,S.singleton x) <$> lookup "js_o" x) (S.toList allDeps)
  return $ (allDeps, M.toList (M.fromListWith S.union allPaths))

-- | collect dependencies for a set of roots
collectDeps :: (String -> Fun -> IO FilePath)
            -> Set Fun
            -> IO (Set Fun, JStat, [ClosureInfo])
collectDeps lookup roots = do
  (allDeps, srcs) <- getDepsSources lookup roots
  (stats, infos) <- unzip <$> mapM (uncurry extractDeps) srcs
  return (allDeps, mconcat stats, concat infos)

extractDeps :: FilePath -> Set Fun -> IO (JStat, [ClosureInfo])
extractDeps file funs = do
  let symbs = S.fromList $ map funSymbol (S.toList funs)
  l <- readObjectFileKeys (any (`S.member` symbs)) file
  return (mconcat (map oiStat l), concatMap oiClInfo l)

pkgTxt :: Package -> Text
pkgTxt p = packageName p <> "-" <> packageVersion p

-- Fun -> packagename-packagever
funPkgTxt :: Fun -> Text
funPkgTxt = pkgTxt . funPackage

-- Fun -> packagename
funPkgTxtNoVer :: Fun -> Text
funPkgTxtNoVer = packageName . funPackage

-- dependencies for the RTS, these need to be always linked
rtsDeps :: [PackageId] -> Set Fun
rtsDeps pkgs =
 let mkDep (p,m,s) = Fun (Package p "") m s
     pkgs'     = map packageIdString pkgs
     pkgErr p  = error ("Package `" ++ p ++ "' is required for linking, but was not found")
     findPkg p | null (filter (p `isPrefixOf`) pkgs') = pkgErr p
               | otherwise                            = T.pack p
     ghcjsPrimPkg   = findPkg "ghcjs-prim"
     ghcPrimPkg     = findPkg "ghc-prim"
     basePkg        = findPkg "base"
     
 in S.fromList $ map mkDep
     [ (basePkg,      "GHC.Conc.Sync",          "h$baseZCGHCziConcziSynczireportError")
     , (basePkg,      "Control.Exception.Base", "h$baseZCControlziExceptionziBasezinonTermination" )
     , (basePkg,      "GHC.Exception",          "h$baseZCGHCziExceptionziSomeException_con_e")
     , (ghcPrimPkg,   "GHC.Types",              "h$ghczmprimZCGHCziTypesziZC_con_e")
     , (ghcPrimPkg,   "GHC.Types",              "h$ghczmprimZCGHCziTypesziZMZN")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimziJSRef_con_e")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimziJSException")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimzizdfExceptionJSExceptionzuzdTypeable")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimzizdfShowJSException")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimzizdfExceptionJSException")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimzizdTypeableJSException")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimzizdfExceptionJSExceptionzuzdShow")
     ]
