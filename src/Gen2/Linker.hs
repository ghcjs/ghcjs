{-# LANGUAGE DefaultSignatures,
             OverloadedStrings,
             TupleSections,
             LambdaCase,
             TemplateHaskell #-}
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
import           Control.Parallel.Strategies

import           Data.Array
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Char                (isDigit)
import qualified Data.Foldable            as F
import           Data.Function            (on)
import           Data.Int
import           Data.List                ( partition, isSuffixOf, isPrefixOf, nub
                                          , intercalate, group, sort, groupBy)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromMaybe, listToMaybe, isJust)
import           Data.Monoid
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import qualified Data.IntSet              as IS
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.IO        as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import qualified Data.Vector              as V

import           Language.Javascript.JMacro
import           Language.Haskell.TH
import           System.FilePath          (dropExtension, splitPath, (<.>), (</>))
import           System.Directory         (createDirectoryIfMissing, doesFileExist)
import           Text.PrettyPrint.Leijen.Text (displayT, renderPretty)

import           DynFlags
import           Config
import           Module                   (PackageId, packageIdString, ModuleName, moduleNameString)

import           Compiler.Info
import           Compiler.Settings

import           Gen2.StgAst
import           Gen2.Rts                 (rtsText)
import           Gen2.Shim
import           Gen2.Printer             (pretty)
import qualified Gen2.Compactor           as Compactor
import           Gen2.ClosureInfo         hiding (Fun)
import           Gen2.Object
import           Gen2.Utils
import           Gen2.RtsTypes

type LinkableUnit = (Package, Module, Int) -- module and the index of the block
type Module       = Text

-- number of bytes linked per module
type LinkerStats  = Map (Package, Module) Int64

link :: DynFlags
     -> GhcjsSettings
     -> FilePath                  -- ^ output file/directory
     -> [FilePath]                -- ^ include path for home package
     -> [(PackageId, [FilePath])] -- ^ directories to load package modules
     -> [FilePath]                -- ^ the object files we're linking
     -> [FilePath]                -- ^ extra js files to include
     -> (Fun -> Bool)             -- ^ functions from the objects to use as roots (include all their deps)
     -> IO [String]               -- ^ arguments for the closure compiler to minify our result
link dflags settings out include pkgs objFiles jsFiles isRootFun = do
  objDeps <- mapM readDepsFile objFiles
  let genBase = isJust (gsGenBase settings)
      jsExt | genBase   = "base.js"
            | otherwise = "js"
      debug = gsDebug settings
      rootTest | Just baseMod <- gsGenBase settings =
                   \(Fun p m s) -> m == T.pack baseMod
               | otherwise = isRootFun
      roots = S.fromList . filter rootTest $
        concatMap (map fst . M.toList . depsDeps) objDeps
      rootMods = map (T.unpack . head) . group . sort . map funModule . S.toList $ roots
  -- putStrLn ("objects: " ++ show (traverse . _1 %~ packageIdString $ pkgs))
  compilationProgressMsg dflags $
    case gsGenBase settings of
      Just baseMod -> "Linking base bundle " ++ out ++ " (" ++ baseMod ++ ")"
      _            -> "Linking " ++ out ++ " (" ++ intercalate "," rootMods ++ ")"
  base <- Compactor.loadBase (gsUseBase settings)
  c <- newMVar M.empty
  (allDeps, code) <-
    collectDeps (lookupFun c $ zip objDeps objFiles)
                (Compactor.baseUnits base)
                (roots `S.union` rtsDeps (map fst pkgs))
  createDirectoryIfMissing False out
  let (outJs, metaSize, renamerState, stats) = renderLinker settings (Compactor.baseRenamerState base) code
      pkgs' = filter (\p -> T.pack (packageIdString p) `notElem` Compactor.basePkgs base) (map fst pkgs)
      pkgsT = map (T.pack . packageIdString) pkgs'
  BL.writeFile (out </> "out" <.> jsExt) outJs
  when (not $ gsOnlyOut settings) $ do
    TL.writeFile (out </> "out.stats") (linkerStats metaSize stats)
    getShims jsFiles pkgs' (out </> "lib" <.> jsExt, out </> "lib1" <.> jsExt)
    if genBase
      then generateBase out base allDeps pkgsT renamerState
      else do
        TL.writeFile (out </> "rts.js") (rtsText' debug)
        when (not 
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
              -> String -> Package -> Module -> IO FilePath
    lookupFun cache objs = {-# SCC "lookupFun" #-} lCached
      where
        lCached :: String -> Package -> Module -> IO FilePath
        lCached ext pkg mod = do
          c <- takeMVar cache
          let k = (pkg, mod, ext)
          case M.lookup k c of
            Just p -> putMVar cache c >> return p
            Nothing -> do
              p <- l ext pkg mod
              -- putStrLn ("looked up: " ++ p)
              putMVar cache (M.insert k p c)
              return p
        objs' = M.fromList $ map (\(Deps pkg m _ _, p) -> ((pkg,m),p)) objs
        l ext pkg mod
           -- already loaded objects
           | Just p    <- M.lookup (pkg, mod) objs' = return p
           -- known package in dependencies
           | Just pkg' <- M.lookup (pkgTxt pkg) pkgPaths = searchPaths [] pkg'
           -- known wired-in package (no version)
           | Just pkg' <- M.lookup (packageName pkg) pkgPaths = searchPaths [] pkg'
           -- search in include dirs
           | otherwise = searchPaths [] include
           where
             modPath = (T.unpack $ T.replace "." "/" mod) <.> ext
             searchPaths searched [] = error $ "cannot find module: " ++ T.unpack (pkgTxt pkg <> ":" <> mod)
                                            ++ "\nsearched in:\n" ++ unlines searched
             searchPaths searched (x:xs) =
                let p = x </> modPath
                in  doesFileExist p >>=
                      \case
                         False -> searchPaths (p:searched) xs
                         True  -> return p

renderLinker :: GhcjsSettings
             -> Compactor.RenamerState
             -> [(Package, Module, JStat, [ClosureInfo])] -- ^ linked code per module
             -> (BL.ByteString, Int64, Compactor.RenamerState, LinkerStats)
renderLinker settings renamerState code =
  let (renamerState', compacted, meta) = Compactor.compact settings renamerState (map (\(_,_,s,ci) -> (s,ci)) code)
      pe = TLE.encodeUtf8 . (<>"\n") . displayT . renderPretty 0.8 150 . pretty
      rendered  = parMap rdeepseq pe compacted
      renderedMeta = pe meta
      mkStat (p,m,_,_) b = ((p,m), BL.length b)
  in ( mconcat rendered <> renderedMeta
     , BL.length renderedMeta
     , renamerState'
     , M.fromList $ zipWith mkStat code rendered
     )

linkerStats :: Int64         -- ^ code size of packed metadata
            -> LinkerStats   -- ^ code size per module
            -> TL.Text
linkerStats meta s =
  TL.intercalate "\n\n" [packageStats, moduleStats, metaStats] <> "\n\n"
  where
    ps = M.fromListWith (+) . map (\((p,_),s) -> (p,s)) . M.toList $ s
    pad n t = let l = TL.length t
              in  if l < n then t <> TL.replicate (n-l) " " else t
    pkgMods = groupBy ((==) `on` (fst . fst)) (M.toList s)
    showMod ((_,m),s) = pad 40 ("    " <> TL.fromStrict m <> ":") <> TL.pack (show s)
    packageStats = "code size summary per package:\n\n" <>
      TL.unlines (map (\(p,s) -> pad 25 (showPkg p <> ":") <> TL.pack (show s)) $ M.toList ps)
    moduleStats = "code size per module:\n\n" <> TL.unlines
      (map (\xs@(((p,_),_):_) -> showPkg p <> "\n" <> TL.unlines (map showMod xs)) pkgMods)
    metaStats = "packed metadata: " <> TL.pack (show meta)

rtsText' :: Bool -> TL.Text
rtsText' = rtsText
{- prerender RTS for faster linking (fixme this results in a build error, why?)
rtsText' debug = if debug
                   then TL.pack $ $(runQ $ litE (StringL . TL.unpack . rtsText $ True))
                   else TL.pack $ $(runQ $ litE (StringL . TL.unpack . rtsText $ False))
-}

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

convertPkg :: PackageId -> (Text, Version)
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
modFuns (Deps p m a d) = map fst (M.toList d)

-- | get all dependencies for a given set of roots
getDeps :: (String -> Package -> Module -> IO FilePath)
        -> Set LinkableUnit -- Fun -- ^ don't link these symbols
        -> Set Fun -- ^ start here
        -> IO (Set LinkableUnit)
getDeps lookup base fun = go' S.empty M.empty [] $
           {- filter (`S.notMember` base) -} (S.toList fun)
  where
    go :: Set LinkableUnit
       -> Map (Package, Module) Deps
       -> [LinkableUnit]
       -> IO (Set LinkableUnit)
    go result _    [] = return result
    go result deps lls@((lpkg,lmod,n):ls) =
      let key = (lpkg, lmod)
      in  case M.lookup (lpkg,lmod) deps of
            Nothing -> lookup "js_o" lpkg lmod >>= readDepsFile >>=
                         \d -> go result (M.insert key d deps) lls
            Just (Deps _ _ a _) -> go' result deps ls (S.toList $ a ! n)

    go' :: Set LinkableUnit
        -> Map (Package, Module) Deps
        -> [LinkableUnit]
        -> [Fun]
        -> IO (Set LinkableUnit)
    go' result deps open [] = go result deps open
    go' result deps open ffs@(f:fs) =
        let key = (funPackage f, funModule f)
        in  case M.lookup key deps of
            Nothing -> lookup "js_o" (funPackage f) (funModule f) >>= readDepsFile >>=
                           \d -> go' result (M.insert key d deps) open ffs
            Just (Deps p m a d) ->
               let lu = maybe err (p,m,) (M.lookup f d)
                   -- fixme, deps include nonexported symbols,
                   -- add error again when those have been removed
                   err = (p,m,-1)
                   -- err = trace ("getDeps: unknown symbol: " ++ showFun f) (p,m,-1)
               in if lu `S.member` result || (\(_,_,n) -> n== -1) lu || lu `S.member` base
                    then go' result deps open fs
                    else go' (S.insert lu result) deps (lu:open) fs

-- | get all modules used by the roots and deps
getDepsSources :: (String -> Package -> Module -> IO FilePath)
               -> Set LinkableUnit -- Fun
               -> Set Fun
               -> IO (Set LinkableUnit, [(FilePath, Set LinkableUnit)])
getDepsSources lookup base roots = do
  allDeps <- getDeps lookup base roots
  allPaths <- mapM (\x@(pkg,mod,_) ->
    (,S.singleton x) <$> lookup "js_o" pkg mod) (S.toList allDeps)
  return $ (allDeps, M.toList (M.fromListWith S.union allPaths))

-- | collect dependencies for a set of roots
collectDeps :: (String -> Package -> Module -> IO FilePath)
            -> Set LinkableUnit -- Fun -- ^ do not include these
            -> Set Fun -- ^ roots
            -> IO (Set LinkableUnit, [(Package, Module, JStat, [ClosureInfo])])
collectDeps lookup base roots = do
  (allDeps, srcs0) <- getDepsSources lookup base roots
  -- read ghc-prim first, since we depend on that for static initialization
  let (primSrcs, srcs) = partition isPrimSrc srcs0
      isPrimSrc (_, fs) = (=="ghc-prim") . (\(p,_,_) -> packageName p) . head . S.toList $ fs
  code <- mapM (uncurry extractDeps) (primSrcs ++ srcs)
  return (allDeps, code)

extractDeps :: FilePath
            -> Set LinkableUnit
            -> IO (Package, Module, JStat, [ClosureInfo])
extractDeps file units = do
  let symbs     = IS.fromList . map (\(_,_,n) -> n) . S.toList $ units
      (p, m, _) = S.elemAt 0 units
  l <- readObjectFileKeys (\n _ -> n `IS.member` symbs) file
  return (p, m, mconcat (map oiStat l), concatMap oiClInfo l)

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
     , (basePkg,      "GHC.Exception",          "h$baseZCGHCziExceptionziSomeException")
     , (basePkg,      "GHC.TopHandler",         "h$baseZCGHCziTopHandlerzirunMainIO")
     , (basePkg,      "GHC.Base",               "h$baseZCGHCziBasezizdfMonadIO")
     , (ghcPrimPkg,   "GHC.Types",              "h$ghczmprimZCGHCziTypesziZC")
     , (ghcPrimPkg,   "GHC.Types",              "h$ghczmprimZCGHCziTypesziZMZN")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimziJSRef")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimziJSException")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimzizdfTypeableJSException")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimzizdfShowJSException")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimzizdfExceptionJSException")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimzizdfTypeableJSException")
     , (ghcjsPrimPkg, "GHCJS.Prim",             "h$ghcjszmprimZCGHCJSziPrimzizdfShowJSException")
     ]

generateBase :: FilePath -> Compactor.Base -> Set LinkableUnit
             -> [Text] -> Compactor.RenamerState -> IO ()
generateBase outDir oldBase funs pkgs rs = do
  BL.writeFile (outDir </> "out.base.symbs") $
    Compactor.renderBase rs (nub $ pkgs ++ Compactor.basePkgs oldBase)
                            (funs `S.union` Compactor.baseUnits oldBase)
