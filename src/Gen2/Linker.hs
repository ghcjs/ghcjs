{-# LANGUAGE DefaultSignatures,
             OverloadedStrings,
             TupleSections,
             LambdaCase,
             DeriveGeneric,
             TemplateHaskell #-}
{- |
  GHCJS linker, collects dependencies from
    the object files (.js_o, js_p_o), which contain linkable
    units with dependency information
-}

module Gen2.Linker where

import           DynFlags
import           Encoding
import           Module                   (PackageId, packageIdString, stringToPackageId)

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Lens             hiding ((<.>))
import           Control.Monad
import           Control.Parallel.Strategies

import           Data.Array
import           Data.Binary
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Char                (toLower)
import           Data.Function            (on)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM
import           Data.Int
import qualified Data.IntSet              as IS
import           Data.List                (partition, isPrefixOf, isSuffixOf, nub, foldl'
                                          ,intercalate, group, sort, groupBy, find, isInfixOf)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromMaybe, isJust, isNothing)
import           Data.Monoid
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import           Data.String              (fromString)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.IO        as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import qualified Data.Vector              as V

import           Data.Yaml                (FromJSON(..), Value(..))
import qualified Data.Yaml                as Yaml

import qualified Distribution.Simple.Utils as Cabal
import qualified Distribution.Verbosity    as Cabal

import           GHC.Generics

import           System.FilePath          (splitPath, (<.>), (</>), dropExtension, takeExtension)
import           System.Directory         (createDirectoryIfMissing, doesDirectoryExist
                                          ,canonicalizePath
                                          ,doesFileExist, getDirectoryContents
                                          ,getCurrentDirectory, copyFile)

import           Text.PrettyPrint.Leijen.Text (displayT, renderPretty)

import           Compiler.Info
import           Compiler.JMacro
import           Compiler.Settings
import           Compiler.Utils

import           Gen2.Base
import           Gen2.ClosureInfo         hiding (Fun)
import qualified Gen2.Compactor           as Compactor
import           Gen2.Object
import           Gen2.Printer             (pretty)
import           Gen2.Utils
import           Gen2.Rts                 (rtsText)
import           Gen2.RtsTypes
import           Gen2.Shim

type LinkableUnit = (Package, Module, Int) -- ^ module and the index of the block in the object file
type Module       = Text
type LinkedObj    = Either (String, ByteString) FilePath -- ^ can be already loaded

-- number of bytes linked per module
type LinkerStats  = Map (Package, Module) Int64

-- | result of a link pass
data LinkResult = LinkResult
  { linkOut         :: BL.ByteString -- ^ compiled Haskell code
  , linkOutStats    :: LinkerStats   -- ^ statistics about generated code
  , linkOutMetaSize :: Int64         -- ^ size of packed metadata in generated code
  , linkLibB        :: [FilePath]    -- ^ library code to load before RTS
  , linkLibA        :: [FilePath]    -- ^ library code to load after RTS
  , linkBase        :: Base          -- ^ base metadata to use if we want to link incrementally against this result
  } deriving (Generic)

instance Binary LinkResult

-- | link and write result to disk (jsexe directory)
link :: DynFlags
     -> GhcjsSettings
     -> FilePath                  -- ^ output file/directory
     -> [FilePath]                -- ^ include path for home package
     -> [(PackageId, [FilePath])] -- ^ directories to load package modules
     -> [LinkedObj]               -- ^ the object files we're linking
     -> [FilePath]                -- ^ extra js files to include
     -> (Fun -> Bool)             -- ^ functions from the objects to use as roots (include all their deps)
     -> Set Fun                   -- ^ extra symbols to link in
     -> IO ()
link dflags settings out include pkgs objFiles jsFiles isRootFun extraStaticDeps
  | gsNoJSExecutables settings = return ()
  | otherwise = do
      LinkResult lo lstats lmetasize llb lla lbase <-
        link' dflags settings out include pkgs objFiles jsFiles isRootFun extraStaticDeps
      let genBase = isJust (gsGenBase settings)
          jsExt | genBase   = "base.js"
                | otherwise = "js"
      createDirectoryIfMissing False out
      BL.writeFile (out </> "out" <.> jsExt) lo
      when (not $ gsOnlyOut settings) $ do
        when (not $ gsNoStats settings) $ do
          let statsFile = if genBase then "out.base.stats" else "out.stats"
          TL.writeFile (out </> statsFile) (linkerStats lmetasize lstats)
        when (not $ gsNoRts settings) $ do
          TL.writeFile (out </> "rts.js") (rtsText' dflags $ dfCgSettings dflags)
        forM_ [(llb, "lib"), (lla, "lib1")] $ \(l,file) -> do
          BL.writeFile (out </> file <.> jsExt) . BL.fromChunks
            =<< mapM (tryReadShimFile dflags) l
          writeFile (out </> file <.> jsExt <.> "files") (unlines l)
        if genBase
          then generateBase out lbase
          else when (not (gsOnlyOut settings) && not (gsNoRts settings) && not (usingBase settings))
                         (writeRunner settings dflags out >> writeHtml dflags out >> combineFiles dflags out)

-- | link in memory
link' :: DynFlags
      -> GhcjsSettings
      -> String                    -- ^ target (for progress message)
      -> [FilePath]                -- ^ include path for home package
      -> [(PackageId, [FilePath])] -- ^ directories to load package modules
      -> [LinkedObj]               -- ^ the object files we're linking
      -> [FilePath]                -- ^ extra js files to include
      -> (Fun -> Bool)             -- ^ functions from the objects to use as roots (include all their deps)
      -> Set Fun                   -- ^ extra symbols to link in
      -> IO LinkResult
link' dflags settings target include pkgs objFiles jsFiles isRootFun extraStaticDeps = do
      objDeps <- mapM readDepsFile' objFiles
      let debug = buildingDebug dflags
          rootSelector | Just baseMod <- gsGenBase settings =
                           \(Fun p m s) -> m == T.pack baseMod
                       | otherwise = isRootFun
          roots = S.fromList . filter rootSelector $
            concatMap (map fst . M.toList . depsDeps) objDeps
          rootMods = map (T.unpack . head) . group . sort . map funModule . S.toList $ roots
      -- putStrLn ("objects: " ++ show (traverse . _1 %~ packageIdString $ pkgs))
      compilationProgressMsg dflags $
        case gsGenBase settings of
          Just baseMod -> "Linking base bundle " ++ target ++ " (" ++ baseMod ++ ")"
          _            -> "Linking " ++ target ++ " (" ++ intercalate "," rootMods ++ ")"
      base <- case gsUseBase settings of
        NoBase        -> return emptyBase
        BaseFile file -> Compactor.loadBase file
        BaseState b   -> return b
      (_, mk_rds) <- rtsDeps dflags
      let rds = mk_rds (map fst pkgs)
      c   <- newMVar M.empty
      (allDeps, code) <-
        collectDeps (lookupFun c $ zip objDeps objFiles)
                (baseUnits base)
                (roots `S.union` rds `S.union` extraStaticDeps)
      let (outJs, metaSize, compactorState, stats) =
             renderLinker settings dflags (baseCompactorState base) code
          rtsPkgs = if usingBase settings
                      then []
                      else map stringToPackageId ["rts", "rts_" ++ rtsBuildTag dflags]
          pkgs' = rtsPkgs ++ filter (not . (isAlreadyLinked base)) (map fst pkgs)
          base' = Base compactorState (nub $ basePkgs base ++ map (T.pack . packageIdString) pkgs')
                         (allDeps `S.union` baseUnits base)
      libJsFiles <- concat <$> mapM getLibJsFiles (concatMap snd pkgs)
      (shimsBefore, shimsAfter) <- getShims dflags (jsFiles ++ libJsFiles) pkgs'
      return $ LinkResult outJs stats metaSize shimsBefore shimsAfter base'
  where
    isAlreadyLinked :: Base -> PackageId -> Bool
    isAlreadyLinked b pkg = T.pack (packageIdString pkg) `elem` basePkgs b

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

    lookupFun :: MVar (Map (Package, Text, String) LinkedObj)
              -> [(Deps,LinkedObj)]
              -> Package -> Module -> IO LinkedObj
    lookupFun cache objs = {-# SCC "lookupFun" #-} lCached
      where
        lCached :: Package -> Module -> IO LinkedObj
        lCached pkg mod = do
          c <- takeMVar cache
          let ext :: String
              ext =
                if pkgTxt pkg `elem` packageIdToStrings (thisPackage dflags)
                  then objectSuf dflags
                  else buildTag dflags ++ "_o"
              k = (pkg, mod, ext)
          case M.lookup k c of
            Just p -> putMVar cache c >> return p
            Nothing -> do
              p <- l ext pkg mod
              -- putStrLn ("looked up: " ++ either (const $ T.unpack (pkgTxt pkg) ++ ":" ++ T.unpack mod ++ " already loaded") show p)
              putMVar cache (M.insert k p c)
              return p
        objs' = M.fromList $ map (\(Deps pkg m _ _ _, p) -> ((pkg,m),p)) objs
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
                         True  -> return (Right p)

renderLinker :: GhcjsSettings
             -> DynFlags
             -> CompactorState
             -> [(Package, Module, JStat, [ClosureInfo], [StaticInfo])] -- ^ linked code per module
             -> (BL.ByteString, Int64, CompactorState, LinkerStats)
renderLinker settings dflags renamerState code =
  let (renamerState', compacted, meta) = Compactor.compact settings dflags renamerState (map (\(_,_,s,ci,si) -> (s,ci,si)) code)
      pe = TLE.encodeUtf8 . (<>"\n") . displayT . renderPretty 0.8 150 . pretty
      rendered  = parMap rdeepseq pe compacted
      renderedMeta = pe meta
      mkStat (p,m,_,_,_) b = ((p,m), BL.length b)
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

rtsText' :: DynFlags -> CgSettings -> TL.Text
rtsText' = rtsText
{- prerender RTS for faster linking (fixme this results in a build error, why?)
rtsText' debug = if debug
                   then TL.pack $ $(runQ $ litE (StringL . TL.unpack . rtsText $ True))
                   else TL.pack $ $(runQ $ litE (StringL . TL.unpack . rtsText $ False))
-}

splitPath' :: FilePath -> [FilePath]
splitPath' = map (filter (`notElem` "/\\")) . splitPath

getLibJsFiles :: FilePath -> IO [FilePath]
getLibJsFiles pkgLibPath = go pkgLibPath "js"
  where
    go dir file = do
      let df = dir </> file
      j <- isJsFile df
      if j then return [df] else do
        d <- doesDirectoryExist df
        if d && file /= "." && file /= ".."
          then getDirectoryContents df >>=
                 fmap concat . mapM (go df) . sort
          else return []
    isJsFile path = do
      e <- doesFileExist path
      return (e && map toLower (takeExtension path) == ".js")

-- fixme the wired-in package id's we get from GHC we have no version
getShims :: DynFlags -> [FilePath] -> [PackageId] -> IO ([FilePath], [FilePath])
getShims dflags extraFiles pkgDeps = do
  (b,a) <- collectShims (getLibDir dflags </> "shims") (map convertPkg pkgDeps)
  extraFiles' <- mapM canonicalizePath extraFiles
  return (b++extraFiles',a)

convertPkg :: PackageId -> (Text, Version)
convertPkg p =
  let (n,v) = splitVersion . T.pack . packageIdString $ p
  in  (n, fromMaybe [] $ parseVersion v)

{- | convenience: combine lib.js, rts.js, lib1.js, out.js to all.js that can be run
     directly with node.js or SpiderMonkey jsshell
 -}
combineFiles :: DynFlags -> FilePath -> IO ()
combineFiles df fp = do
  files   <- mapM (B.readFile.(fp</>)) ["lib.js", "rts.js", "lib1.js", "out.js"]
  runMain <- B.readFile (getLibDir df </> "runmain.js")
  B.writeFile (fp</>"all.js") (mconcat (files ++ [runMain]))

-- | write the index.html file that loads the program if it does not exit
writeHtml :: DynFlags -> FilePath -> IO ()
writeHtml df out = do
  e <- doesFileExist htmlFile
  unless e $ do
    let libdir = getLibDir df
    when ("-DGHCJS_PROF_GUI" `elem` opt_P df) $
      Cabal.installDirectoryContents Cabal.normal
        (fromString $ libdir </> "shims" </> "lib" </> "polymer-components")
        (fromString $ out </> "polymer-components")
    Cabal.installOrdinaryFile Cabal.normal
      (fromString $ libdir </> "template.html") (fromString htmlFile)
  where
    htmlFile = out </> "index.html"

writeRunner :: GhcjsSettings -> DynFlags -> FilePath -> IO ()
writeRunner settings df out = when (gsBuildingCabalSetup settings) $ do
  cd <- getCurrentDirectory
  let runner    = cd </> addExeExtension (dropExtension out)
      runnerSrc = topDir df </> addExeExtension "ghcjs-run"
  copyFile runnerSrc runner
  Cabal.setFileExecutable runner
  copyFile (topDir df </> "node") (cd </> out </> "node")

-- | drop the version from a package name
dropVersion :: Text -> Text
dropVersion = fst . splitVersion

-- | split a package id into a package name and version
--   warning this might make some assumptions about version numbers
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
modFuns (Deps p m e a d) = map fst (M.toList d)

-- | get all dependencies for a given set of roots
getDeps :: (Package -> Module -> IO LinkedObj)
        -> Set LinkableUnit -- Fun -- ^ don't link these symbols
        -> Set Fun -- ^ start here
        -> IO (Set LinkableUnit)
getDeps lookup base fun = go' S.empty M.empty [] $ S.toList fun
  where
    go :: Set LinkableUnit
       -> Map (Package, Module) Deps
       -> [LinkableUnit]
       -> IO (Set LinkableUnit)
    go result _    [] = return result
    go result deps lls@((lpkg,lmod,n):ls) =
      let key = (lpkg, lmod)
      in  case M.lookup (lpkg,lmod) deps of
            Nothing -> lookup lpkg lmod >>= readDepsFile' >>=
                         \d -> go result (M.insert key d deps) lls
            Just (Deps _ _ _ a _) -> go' result deps ls (S.toList $ a ! n)

    go' :: Set LinkableUnit
        -> Map (Package, Module) Deps
        -> [LinkableUnit]
        -> [Fun]
        -> IO (Set LinkableUnit)
    go' result deps open [] = go result deps open
    go' result deps open ffs@(f:fs) =
        let key = (funPackage f, funModule f)
        in  case M.lookup key deps of
            Nothing -> lookup (funPackage f) (funModule f) >>= readDepsFile' >>=
                           \d -> go' result (M.insert key d deps) open ffs
            Just (Deps p m e a d) ->
               let lu = maybe err (p,m,) (M.lookup f d)
                   -- fixme, deps include nonexported symbols,
                   -- add error again when those have been removed
                   err = (p,m,-1)
                   -- err = trace ("getDeps: unknown symbol: " ++ showFun f) (p,m,-1)
               in if lu `S.member` result || (\(_,_,n) -> n== -1) lu || lu `S.member` base
                    then go' result deps open fs
                    else go' (S.insert lu result) deps (lu:open) fs

-- | get all modules used by the roots and deps
getDepsSources :: (Package -> Module -> IO LinkedObj)
               -> Set LinkableUnit -- Fun
               -> Set Fun
               -> IO (Set LinkableUnit, [(LinkedObj, Set LinkableUnit)])
getDepsSources lookup base roots = do
  allDeps <- getDeps lookup base roots
  allPaths <- mapM (\x@(pkg,mod,_) ->
    (,S.singleton x) <$> lookup pkg mod) (S.toList allDeps)
  return $ (allDeps, M.toList (M.fromListWith S.union allPaths))

-- | collect dependencies for a set of roots
collectDeps :: (Package -> Module -> IO LinkedObj)
            -> Set LinkableUnit -- ^ do not include these
            -> Set Fun -- ^ roots
            -> IO (Set LinkableUnit, [(Package, Module, JStat, [ClosureInfo], [StaticInfo])])
collectDeps lookup base roots = do
  (allDeps, srcs0) <- getDepsSources lookup base roots
  -- read ghc-prim first, since we depend on that for static initialization
  let (primSrcs, srcs) = partition isPrimSrc srcs0
      isPrimSrc (_, fs) = (=="ghc-prim") . (\(p,_,_) -> packageName p) . head . S.toList $ fs
  code <- mapM (uncurry extractDeps) (primSrcs ++ srcs)
  return (allDeps, code)

extractDeps :: LinkedObj
            -> Set LinkableUnit
            -> IO (Package, Module, JStat, [ClosureInfo], [StaticInfo])
extractDeps obj units = do
  let symbs        = IS.fromList . map (\(_,_,n) -> n) . S.toList $ units
      (p, m, _)    = S.elemAt 0 units
      selector n _ = n `IS.member` symbs || isGlobalUnit n
  l <- either (\(n,b) -> return $ readObjectKeys n selector (BL.fromStrict b))
              (readObjectFileKeys selector)
              obj
  return (p, m, mconcat (map oiStat l), concatMap oiClInfo l, concatMap oiStatic l)

pkgTxt :: Package -> Text
pkgTxt p | T.null (packageVersion p) = packageName p
         | otherwise                 = packageName p <> "-" <> packageVersion p

-- | Fun -> "packagename-packagever"
funPkgTxt :: Fun -> Text
funPkgTxt = pkgTxt . funPackage

-- | Fun -> "packagename"
funPkgTxtNoVer :: Fun -> Text
funPkgTxtNoVer = packageName . funPackage

{- | Static dependencies are symbols that need to be linked regardless
     of whether the linked program refers to them. For example
     depenencies that the RTS uses or symbols that the user program
     refers to directly
 -}
newtype StaticDeps =
  StaticDeps { unStaticDeps :: [(Text, Text, Text)] -- package/module/symbol
             }

noStaticDeps :: StaticDeps
noStaticDeps = StaticDeps []

{- | The input file format for static deps is a yaml document with a
     package/module/symbol tree where symbols can be either a list or
     just a single string, for example:

     base:
       GHC.Conc.Sync:          reportError
       Control.Exception.Base: nonTermination
     ghcjs-prim:
       GHCJS.Prim:
         - JSRef
         - JSException
 -}
instance FromJSON StaticDeps where
  parseJSON (Object v) = StaticDeps . concat <$> mapM (uncurry parseMod) (HM.toList v)
    where
      parseMod p (Object v) = concat <$> mapM (uncurry (parseSymb p)) (HM.toList v)
      parseMod _ _          = mempty
      parseSymb p m (String s) = pure [(p,m,s)]
      parseSymb p m (Array v)  = mapM (parseSingleSymb p m) (V.toList v)
      parseSymb _ _ _          = mempty
      parseSingleSymb p m (String s) = pure (p,m,s)
      parseSingleSymb _ _ _          = mempty
  parseJSON _          = mempty

-- | dependencies for the RTS, these need to be always linked
rtsDeps :: DynFlags -> IO ([Text], [PackageId] -> Set Fun)
rtsDeps = readSystemDeps "RTS" "linking" "rtsdeps.yaml"

-- | dependencies for the Template Haskell, these need to be linked when running
--   Template Haskell (in addition to the RTS deps)
thDeps :: DynFlags -> IO ([Text], [PackageId] -> Set Fun)
thDeps = readSystemDeps "Template Haskell" "running Template Haskell" "thdeps.yaml"

readSystemDeps :: String
               -> String
               -> FilePath
               -> DynFlags
               -> IO ([Text], [PackageId] -> Set Fun)
readSystemDeps depsName requiredFor file df = do
  b <- B.readFile (getLibDir df </> file)
  case Yaml.decodeEither b of
    Left err -> error ("could not read " ++ depsName ++ " dependencies from " ++ file ++ ":\n" ++ err)
    Right sdeps -> return (depsPkgs sdeps, staticDeps' sdeps)
  where
    depsFile = getLibDir df </> file
    depsPkgs s = map head . group . sort $ map (^._1) (unStaticDeps s)
    staticDeps' s pkgs
      | StaticDeps ((p,_,_):_) <- unresolved =
          error ("Package `" ++ T.unpack p ++ "' is required for " ++ requiredFor ++ ", but was not found")
      | otherwise = funs
      where (unresolved, funs) = staticDeps s pkgs

{- | read a static dependencies specification and give the roots

     if dependencies come from a versioned (non-hardwired) package
     that is linked multiple times, then the returned dependencies
     will all come from the same version, but it's undefined which one.
  -}
staticDeps :: StaticDeps              -- ^ deps from yaml file
           -> [PackageId]             -- ^ packages we're using
           -> (StaticDeps, (Set Fun)) -- ^ the StaticDeps contains the symbols for which no package could be found
staticDeps sdeps pkgs = mkDeps sdeps
  where
    zenc  = T.pack . zEncodeString . T.unpack
    pkgsT = map pTxt pkgs
    pTxt p | isWiredInPackage xs = dropVersion (T.pack xs)
           | otherwise           = T.pack xs
      where xs = packageIdString p
    pkgsU = fmap splitVersion pkgsT
    mkDeps (StaticDeps ds) =
      (\(_,u,r) -> (StaticDeps u,r)) (foldl' resolveDep (HM.empty, [], S.empty) ds)
    resolveDep (pkgMap, unresolved, resolved) dep@(p, m, s)
      | Just (p', pkgMap') <- mkPkg pkgMap p =
          (pkgMap', unresolved, S.insert (Fun p' m $ mkSymb p' m s) resolved)
      | otherwise = (pkgMap, dep : unresolved, resolved)
    mkSymb p m s  = "h$" <> zenc (pkgTxt p <> ":" <> m <> "." <> s)
    mkPkg pm p
      | Just p' <- HM.lookup p pm = Just (p', pm)
      | Just pl <- findPkg p =
          let p' = uncurry Package pl
          in  Just (p', HM.insert p p' pm)
      | otherwise = Nothing
    findPkg p =
      let (pn,pv) = splitVersion p
      in  msum [ splitVersion <$> find (==p) pkgsT -- full name matches
                                                   -- name and version prefix match
               , find (\(pn',pv') -> pn' == pn && (T.null pv || (pv <> ".") `T.isPrefixOf` (pv' <> "."))) pkgsU
               ]

-- read dependencies from an object that might have already been loaded
readDepsFile' :: LinkedObj -> IO Deps
readDepsFile' (Left (name, bs)) = pure (readDeps name (BL.fromStrict bs))
readDepsFile' (Right file)      = readDepsFile file

generateBase :: FilePath -> Base -> IO ()
generateBase outDir b =
  BL.writeFile (outDir </> "out.base.symbs") (Compactor.renderBase b)


