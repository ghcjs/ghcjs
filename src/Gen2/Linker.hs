{-# LANGUAGE CPP,
             DefaultSignatures,
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
import           Panic
#if __GLASGOW_HASKELL__ >= 709
import           Module (mkModuleName, wiredInPackageKeys)
import           PackageConfig (sourcePackageId, packageKey)
#else
import           UniqFM
import           Module (mkModuleName)
import qualified Module as Mod
import           PackageConfig (sourcePackageId)
import           Data.Maybe (listToMaybe)
import           Distribution.Package (InstalledPackageId(..))
#endif
import           Module (moduleNameString)
import           Outputable (ppr, showSDoc)
import qualified Packages
import qualified SysTools

import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Control.Exception        (evaluate)
import           Control.Lens             hiding ((<.>))
import           Control.Monad
import           Control.Parallel.Strategies

import           Data.Array
import           Data.Binary
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Char                (toLower, chr)
import           Data.Function            (on)
import qualified Data.HashMap.Strict      as HM
import           Data.Int
import           Data.IntSet              (IntSet)
import qualified Data.IntSet              as IS
import           Data.List
  (partition, nub, foldl', intercalate, group, sort, groupBy, find)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe
import           Data.Monoid
import           Data.Set                 (Set)
import qualified Data.Set                 as S
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

import           Numeric                  (showOct)

import           GHC.Generics

import           System.FilePath
  (splitPath, (<.>), (</>), dropExtension, takeExtension)

import           System.Directory
  (createDirectoryIfMissing, doesDirectoryExist, canonicalizePath
  ,doesFileExist, getDirectoryContents, getCurrentDirectory, copyFile)
import           Text.PrettyPrint.Leijen.Text (displayT, renderPretty)

import           Compiler.Compat
import           Compiler.Info
import           Compiler.JMacro
import           Compiler.Settings
import           Compiler.Utils

import qualified Gen2.Archive             as Ar
import           Gen2.Base
import           Gen2.ClosureInfo         hiding (Fun)
import qualified Gen2.Compactor           as Compactor
import           Gen2.Object
import           Gen2.Printer             (pretty)
import           Gen2.Rts                 (rtsText, rtsDeclsText)
import           Gen2.RtsTypes
import           Gen2.Shim
import           System.Exit (exitFailure)

type LinkableUnit = (Package, Module, Int) -- ^ module and the index of the block in the object file
type Module       = Text

-- number of bytes linked per module
type LinkerStats  = Map (Package, Module) Int64

-- | result of a link pass
data LinkResult = LinkResult
  { linkOut         :: BL.ByteString -- ^ compiled Haskell code
  , linkOutStats    :: LinkerStats   -- ^ statistics about generated code
  , linkOutMetaSize :: Int64         -- ^ size of packed metadata in generated code
  , linkLibRTS      :: [FilePath]    -- ^ library code to load with the RTS
  , linkLibA        :: [FilePath]    -- ^ library code to load after RTS
  , linkLibAArch    :: [FilePath]    -- ^ library code to load from archives after RTS
  , linkBase        :: Base          -- ^ base metadata to use if we want to link incrementally against this result
  } deriving (Generic)

instance Binary LinkResult


-- | link and write result to disk (jsexe directory)
link :: DynFlags
     -> GhcjsEnv
     -> GhcjsSettings
     -> FilePath                   -- ^ output file/directory
     -> [FilePath]                 -- ^ include path for home package
     -> [PackageKey]               -- ^ packages to link
     -> [LinkedObj]                -- ^ the object files we're linking
     -> [FilePath]                 -- ^ extra js files to include
     -> (Fun -> Bool)              -- ^ functions from the objects to use as roots (include all their deps)
     -> Set Fun                    -- ^ extra symbols to link in
     -> IO ()
link dflags env settings out include pkgs objFiles jsFiles isRootFun extraStaticDeps
  | gsNoJSExecutables settings = return ()
  | otherwise = do
      LinkResult lo lstats lmetasize llW lla llarch lbase <-
        link' dflags env settings out include pkgs objFiles jsFiles
              isRootFun extraStaticDeps
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
          withRts <- mapM (tryReadShimFile dflags) llW
          BL.writeFile (out </> "rts.js")
            (TLE.encodeUtf8 rtsDeclsText <>
             BL.fromChunks withRts <>
             TLE.encodeUtf8 (rtsText' dflags $ dfCgSettings dflags))
        lla'    <- mapM (tryReadShimFile dflags) lla
        llarch' <- mapM (readShimsArchive dflags) llarch
        BL.writeFile (out </> "lib" <.> jsExt)
                     (BL.fromChunks $ lla' ++ llarch')
        if genBase
          then generateBase out lbase
          else when (not (gsOnlyOut settings) &&
                     not (gsNoRts settings) &&
                     not (usingBase settings)) $ do
                 combineFiles dflags out
                 writeHtml dflags out
                 writeRunMain dflags out
                 writeRunner settings dflags out
                 writeWebAppManifest dflags out

-- | link in memory
link' :: DynFlags
      -> GhcjsEnv
      -> GhcjsSettings
      -> String                     -- ^ target (for progress message)
      -> [FilePath]                 -- ^ include path for home package
      -> [PackageKey]               -- ^ packages to link
      -> [LinkedObj]                -- ^ the object files we're linking
      -> [FilePath]                 -- ^ extra js files to include
      -> (Fun -> Bool)              -- ^ functions from the objects to use as roots (include all their deps)
      -> Set Fun                    -- ^ extra symbols to link in
      -> IO LinkResult
link' dflags env settings target include pkgs objFiles jsFiles isRootFun extraStaticDeps = do
      (objDepsMap, objRequiredUnits) <- loadObjDeps objFiles
      let rootSelector | Just baseMod <- gsGenBase settings =
                           \(Fun p m s) -> m == T.pack baseMod
                       | otherwise = isRootFun
          roots = S.fromList . filter rootSelector $
            concatMap (M.keys . depsHaskellExported . fst) (M.elems objDepsMap)
          rootMods = map (T.unpack . head) . group . sort . map funModule . S.toList $ roots
          objPkgs = map toPackageKey $ nub (map fst $ M.keys objDepsMap)
      -- putStrLn ("objects: " ++ show (traverse . _1 %~ packageKeyString $ pkgs))
      compilationProgressMsg dflags $
        case gsGenBase settings of
          Just baseMod -> "Linking base bundle " ++ target ++ " (" ++ baseMod ++ ")"
          _            -> "Linking " ++ target ++ " (" ++ intercalate "," rootMods ++ ")"
      base <- case gsUseBase settings of
        NoBase        -> return emptyBase
        BaseFile file -> Compactor.loadBase file
        BaseState b   -> return b
      (rdPkgs, rds) <- rtsDeps dflags
      c   <- newMVar M.empty
      let rtsPkgs     =  map stringToPackageKey
                             ["@rts", "@rts_" ++ rtsBuildTag dflags]
          pkgs'       = nub (rtsPkgs ++ rdPkgs ++ objPkgs ++ pkgs)
          pkgs''      = filter (not . (isAlreadyLinked base)) pkgs'
          pkgLibPaths = mkPkgLibPaths pkgs'
          getPkgLibPaths :: PackageKey -> ([FilePath],[String])
          getPkgLibPaths k = fromMaybe ([],[]) (M.lookup k pkgLibPaths)
      (archsDepsMap, archsRequiredUnits) <- loadArchiveDeps env =<<
          getPackageArchives dflags (M.elems $ mkPkgLibPaths pkgs')
      pkgArchs <- getPackageArchives dflags (M.elems $ mkPkgLibPaths pkgs'')
      (allDeps, code) <-
        collectDeps dflags
                    (objDepsMap `M.union` archsDepsMap)
                    (pkgs' ++ [thisPackage dflags])
                    (baseUnits base)
                    (roots `S.union` rds `S.union` extraStaticDeps)
                    (archsRequiredUnits ++ objRequiredUnits)
      let (outJs, metaSize, compactorState, stats) =
             renderLinker settings dflags (baseCompactorState base) code
          base'  = Base compactorState (nub $ basePkgs base ++ map mkPackage pkgs'')
                         (allDeps `S.union` baseUnits base)
      (alreadyLinkedBefore, alreadyLinkedAfter) <- getShims dflags [] (filter (isAlreadyLinked base) pkgs')
      (shimsBefore, shimsAfter) <- getShims dflags jsFiles pkgs''
      return $ LinkResult outJs stats metaSize
                 (filter (`notElem` alreadyLinkedBefore) shimsBefore)
                 (filter (`notElem` alreadyLinkedAfter)  shimsAfter)
                 pkgArchs base'
  where
    isAlreadyLinked :: Base -> PackageKey -> Bool
    isAlreadyLinked b pkg = mkPackage pkg `elem` basePkgs b

    mkPkgLibPaths :: [PackageKey] -> Map PackageKey ([FilePath],[String])
    mkPkgLibPaths
      = M.fromList
      . map (\k -> ( k
                   , (getPackageLibDirs dflags k, getPackageHsLibs dflags k)
                   ))

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
splitPath' = map (filter (`notElem` ("/\\"::String))) . splitPath

getPackageArchives :: DynFlags -> [([FilePath],[String])] -> IO [FilePath]
getPackageArchives dflags pkgs =
  filterM doesFileExist [ p </> "lib" ++ l ++ profSuff <.> "js_a"
                        | (paths, libs) <- pkgs, p <- paths, l <- libs ]
  where
    profSuff | WayProf `elem` ways dflags = "_p"
             | otherwise                  = ""

-- fixme the wired-in package id's we get from GHC we have no version
getShims :: DynFlags -> [FilePath] -> [PackageKey] -> IO ([FilePath], [FilePath])
getShims dflags extraFiles pkgDeps = do
  (w,a) <- collectShims (getLibDir dflags </> "shims")
                        (map (convertPkg dflags) pkgDeps)
  extraFiles' <- mapM canonicalizePath extraFiles
  return (w, a++extraFiles')

convertPkg :: DynFlags -> PackageKey -> (Text, Version)
convertPkg dflags p
  = case getPackageVersion dflags p of
      Just v -> (T.pack (getPackageName dflags p), v)
      -- special or wired-in
      Nothing -> (T.pack (packageKeyString p), Version [])

{- | convenience: combine rts.js, lib.js, out.js to all.js that can be run
     directly with node.js or SpiderMonkey jsshell
 -}
combineFiles :: DynFlags -> FilePath -> IO ()
combineFiles df fp = do
  files   <- mapM (B.readFile.(fp</>)) ["rts.js", "lib.js", "out.js"]
  runMain <- B.readFile (getLibDir df </> "runmain.js")
  B.writeFile (fp</>"all.js") (mconcat (files ++ [runMain]))

-- | write the index.html file that loads the program if it does not exit
writeHtml :: DynFlags -> FilePath -> IO ()
writeHtml df out = do
  e <- doesFileExist htmlFile
  when (not e) $
    B.readFile (getLibDir df </>"template.html") >>= B.writeFile htmlFile
  where
    htmlFile = out </> "index.html"

-- | write the runmain.js file that will be run with defer so that it runs after index.html is loaded
writeRunMain :: DynFlags -> FilePath -> IO ()
writeRunMain df out = do
  e <- doesFileExist runMainFile
  when (not e) $
    B.readFile (getLibDir df </> "runmain.js") >>= B.writeFile runMainFile
  where
    runMainFile = out </> "runmain.js"

writeRunner :: GhcjsSettings -> DynFlags -> FilePath -> IO ()
writeRunner settings dflags out = when (gsBuildRunner settings) $ do
  cd    <- getCurrentDirectory
  let runner = cd </> addExeExtension (dropExtension out)
#ifdef mingw32_HOST_OS
  src   <- B.readFile (cd </> out </> "all" <.> "js")
  node  <- B.readFile (topDir dflags </> "node")
  templ <- T.readFile (topDir dflags </> "runner.c-tmpl")
  runnerSrc <- SysTools.newTempName dflags "c"
  T.writeFile runnerSrc $
    substPatterns [] [ ("js",     bsLit src)
                     , ("jsSize", T.pack (show $ B.length src))
                     , ("node",   bsLit node)
                     ] templ
  SysTools.runCc dflags [ Option "-o"
                        , FileOption "" runner
                        , FileOption "" runnerSrc
			, FileOption "" (topDir dflags </> "runner-resources.o")
                        ]
    where
      bsLit b = T.pack $ '"' : concatMap escapeChar (B.unpack b) ++ "\""
      escapeChar  9 = "\\t"
      escapeChar 10 = "\\n"
      escapeChar 13 = "\\r"
      escapeChar 34 = "\\\""
      escapeChar 63 = "\\?"
      escapeChar 92 = "\\\\"
      escapeChar x
        | x >= 32 && x <= 127 = chr (fromIntegral x) : []
        | otherwise           = '\\' : escapeOctal x
      escapeOctal x =
        let x' = showOct x []
        in  replicate (3-length x') '0' ++ x'
#else
  src   <- B.readFile (cd </> out </> "all" <.> "js")
  node  <- B.readFile (topDir dflags </> "node")
  B.writeFile runner ("#!" <> node <> "\n" <> src)
  Cabal.setFileExecutable runner
#endif

-- | write the manifest.webapp file that for firefox os
writeWebAppManifest :: DynFlags -> FilePath -> IO ()
writeWebAppManifest df out = do
  e <- doesFileExist manifestFile
  when (not e) $
    B.readFile (getLibDir df </> "manifest.webapp") >>= B.writeFile manifestFile
  where
    manifestFile = out </> "manifest.webapp"

-- | get all functions in a module
modFuns :: Deps -> [Fun]
modFuns (Deps _p _m _r e _b) = M.keys e

-- | get all dependencies for a given set of roots
getDeps :: Map (Package,Module) Deps -- ^ loaded deps
        -> Set LinkableUnit -- ^ don't link these blocks
        -> Set Fun          -- ^ start here
        -> [LinkableUnit]   -- ^ and also link these
        -> IO (Set LinkableUnit)
getDeps lookup base fun startlu = go' S.empty (S.fromList startlu) (S.toList fun)
  where
    go :: Set LinkableUnit
       -> Set LinkableUnit
       -> IO (Set LinkableUnit)
    go result open = case S.minView open of
      Nothing -> return result
      Just (lu@(lpkg,lmod,n), open') ->
          let key = (lpkg, lmod)
          in  case M.lookup (lpkg,lmod) lookup of
                Nothing -> error ("getDeps.go: object file not loaded for:  " ++ show key)
                Just (Deps _ _ _ _ b) ->
                  let block = b!n
                      result' = S.insert lu result
                  in go' result'
                         (addOpen result' open' $ map (lpkg,lmod,) (blockBlockDeps block))
                         (blockFunDeps block)

    go' :: Set LinkableUnit
        -> Set LinkableUnit
        -> [Fun]
        -> IO (Set LinkableUnit)
    go' result open [] = go result open
    go' result open (f:fs) =
        let key = (funPackage f, funModule f)
        in  case M.lookup key lookup of
              Nothing ->
                if funPackage f == Package "interactive"
                  then go' result open fs
                  else error ("getDeps.go': object file not loaded for:  " ++ show key)
              Just (Deps p m _r e b) ->
                 let lun :: Int
                     lun = fromMaybe (error $ "exported function not found: " ++ show f)
                                     (M.lookup f e)
                     lu  = (funPackage f, funModule f, lun)
                 in  go' result (addOpen result open [lu]) fs

    addOpen :: Set LinkableUnit -> Set LinkableUnit -> [LinkableUnit]
            -> Set LinkableUnit
    addOpen result open newUnits =
      let alreadyLinked s = S.member s result ||
                            S.member s open   ||
                            S.member s base
      in  open `S.union` (S.fromList $ filter (not . alreadyLinked) newUnits)

-- | collect dependencies for a set of roots
collectDeps :: DynFlags
            -> Map (Package, Module) (Deps, DepsLocation)
            -> [PackageKey]     -- ^ packages, code linked in this order
            -> Set LinkableUnit -- ^ do not include these
            -> Set Fun -- ^ roots
            -> [LinkableUnit] -- ^ more roots
            -> IO ( Set LinkableUnit
                  , [(Package, Module, JStat, [ClosureInfo], [StaticInfo])]
                  )
collectDeps dflags lookup packages base roots units = do
  allDeps <- getDeps (fmap fst lookup) base roots units
  -- read ghc-prim first, since we depend on that for static initialization
  let packages' = uncurry (++) $ partition (==primPackageKey) (nub packages)
      unitsByModule :: Map (Package, Module) IntSet
      unitsByModule = M.fromListWith IS.union $
                      map (\(p,m,n) -> ((p,m),IS.singleton n)) (S.toList allDeps)
      lookupByPkg :: Map Package [(Deps, DepsLocation)]
      lookupByPkg = M.fromListWith (++) (map (\((p,m),v) -> (p,[v])) (M.toList lookup))
  code <- fmap (catMaybes . concat) . forM packages' $ \pkg -> do
    mapM (uncurry $ extractDeps unitsByModule)
         (fromMaybe [] $ M.lookup (mkPackage pkg) lookupByPkg)
  return (allDeps, code)

extractDeps :: Map (Package, Module) IntSet
            -> Deps
            -> DepsLocation
            -> IO (Maybe (Package, Module, JStat, [ClosureInfo], [StaticInfo]))
extractDeps units deps loc =
  case M.lookup (pkg, mod) units of
    Nothing       -> return Nothing
    Just modUnits -> do
      let selector n _  = n `IS.member` modUnits || isGlobalUnit n
      x <- case loc of
        ObjectFile js_o  -> collectCode =<< readObjectFileKeys selector js_o
        ArchiveFile js_a -> collectCode =<<
                            (readObjectKeys (js_a ++ ':':T.unpack mod) selector <$>
                            Ar.readObject (mkModuleName $ T.unpack mod) js_a)
        InMemory n b     -> collectCode $
                            readObjectKeys n selector (BL.fromStrict b)
      evaluate (rnf x)
      return x
  where
    pkg           = depsPackage deps
    mod           = depsModule deps
    collectCode l = let x = ( pkg
                            , mod
                            , mconcat (map oiStat l)
                            , concatMap oiClInfo l
                            , concatMap oiStatic l)
                    in evaluate (rnf x) >> return (Just x)

mkPackage :: PackageKey -> Package
mkPackage pk = Package (T.pack $ packageKeyString pk)

toPackageKey :: Package -> PackageKey
toPackageKey = stringToPackageKey . T.unpack . unPackage

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
         - JSVal
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
rtsDeps :: DynFlags -> IO ([PackageKey], Set Fun)
rtsDeps dflags = readSystemDeps dflags
                                "RTS"
                                "linking"
                                "rtsdeps.yaml"

-- | dependencies for the Template Haskell, these need to be linked when running
--   Template Haskell (in addition to the RTS deps)
thDeps :: DynFlags -> IO ([PackageKey], Set Fun)
thDeps dflags = readSystemDeps dflags
                               "Template Haskell"
                               "running Template Haskell"
                               "thdeps.yaml"

readSystemDeps :: DynFlags
               -> String
               -> String
               -> FilePath
               -> IO ([PackageKey], Set Fun)
readSystemDeps dflags depsName requiredFor file = do
  b  <- B.readFile (getLibDir dflags </> file)
  wi <- readSystemWiredIn dflags
  case Yaml.decodeEither b of
    Left err -> error $ "could not read " ++ depsName ++
                        " dependencies from " ++ file ++ ":\n" ++ err
    Right sdeps ->
      let (StaticDeps unresolved, pkgs, funs) = staticDeps dflags wi sdeps
      in  case unresolved of
            xs@((p,_,_):_) -> do
                  error ( "Package `" ++ T.unpack p ++ "' is required for " ++
                          requiredFor ++ ", but was not found")
            _ -> return (pkgs, funs)



readSystemWiredIn :: DynFlags -> IO [(Text, PackageKey)]
readSystemWiredIn dflags = do
  b <- B.readFile filename
  case Yaml.decodeEither b of
     Left err -> error $ "could not read wired-in package keys from " ++ filename
     Right m  -> return . M.toList
                        . M.union ghcWiredIn -- GHC wired-in package keys override those in the file
                        . fmap stringToPackageKey $ m
  where
    filename = getLibDir dflags </> "wiredinkeys" <.> "yaml"
    ghcWiredIn :: Map Text PackageKey
    ghcWiredIn = M.fromList $ map (\k -> (T.pack (packageKeyString k), k))
#if __GLASGOW_HASKELL__ >= 709
                                  wiredInPackageKeys
#else
                                  [ Mod.primPackageId, Mod.integerPackageId, Mod.basePackageId
                                  , Mod.rtsPackageId, Mod.thPackageId, Mod.dphSeqPackageId
                                  , Mod.dphParPackageId, Mod.thisGhcPackageId
                                  ]
#endif
{- | read a static dependencies specification and give the roots

     if dependencies come from a versioned (non-hardwired) package
     that is linked multiple times, then the returned dependencies
     will all come from the same version, but it's undefined which one.
 -}

type SDep = (Text, Text, Text)

staticDeps :: DynFlags
           -> [(Text, PackageKey)]    -- ^ wired-in package names / keys
           -> StaticDeps              -- ^ deps from yaml file
           -> (StaticDeps, [PackageKey], Set Fun)
                                      -- ^ the StaticDeps contains the symbols
                                      --   for which no package could be found
staticDeps dflags wiredin sdeps = mkDeps sdeps
  where
#if !(__GLASGOW_HASKELL__ >= 709)
    lookupInstalledPackage :: Packages.PackageConfigMap -> PackageKey -> Maybe Packages.PackageConfig
    lookupInstalledPackage pkgs ipid =
      case filter ((==InstalledPackageId (packageKeyString ipid)) . Packages.installedPackageId) (eltsUFM pkgs) of
        [conf] -> Just conf
        _      -> Packages.lookupPackage pkgs ipid
#endif
    zenc  = T.pack . zEncodeString . T.unpack
    mkDeps (StaticDeps ds) =
      let (u, p, r) = foldl' resolveDep ([], S.empty, S.empty) ds
      in  (StaticDeps u, S.toList (closePackageDeps dflags p), r)
    resolveDep :: ([SDep], Set PackageKey, Set Fun)
               -> SDep
               -> ([SDep], Set PackageKey, Set Fun)
    resolveDep (unresolved, pkgs, resolved) dep@(p, m, s) =
      case lookup p wiredin of
             Nothing -> ( dep : unresolved, pkgs, resolved)
#if __GLASGOW_HASKELL__ >= 709
             Just k  -> case Packages.lookupPackage dflags k of
#else
             Just k  -> case lookupInstalledPackage (Packages.pkgIdMap . pkgState $ dflags) k of
#endif
               Nothing -> error $ "Package key for wired-in dependency `" ++
                                  T.unpack p ++ "' could not be found: "  ++
                                  packageKeyString k
               Just conf ->
#if __GLASGOW_HASKELL__ >= 709
                 let k' = packageKey conf
#else
                 let k' = Packages.packageConfigId conf
#endif
                 in  ( unresolved
                     , S.insert k' pkgs
                     , S.insert (Fun (mkPackage k') m $ mkSymb k' m s)
                               resolved
                     )
    mkSymb :: PackageKey -> Text -> Text -> Text
    mkSymb p m s  =
      "h$" <> zenc (T.pack (encodePackageKey dflags p) <> ":" <> m <> "." <> s)

closePackageDeps :: DynFlags -> Set PackageKey -> Set PackageKey
closePackageDeps dflags pkgs
  | S.size pkgs == S.size pkgs' = pkgs
  | otherwise                   = closePackageDeps dflags pkgs'
  where
    pkgs' = pkgs `S.union` S.fromList (concatMap deps $ S.toList pkgs)
    notFound = error "closePackageDeps: package not found"
    deps :: PackageKey -> [PackageKey]
    deps =
#if __GLASGOW_HASKELL__ >= 709
           map (Packages.resolveInstalledPackageId dflags)
         . Packages.depends
         . fromMaybe notFound
         . Packages.lookupPackage dflags
#else
           map resolveDep
         . Packages.depends
         . fromMaybe notFound
         . Packages.lookupPackage pkgMap
    pkgMap = Packages.pkgIdMap (pkgState dflags)
    allPkgs = eltsUFM pkgMap
    resolveDep ipid =
      maybe notFound
            Packages.packageConfigId
            (listToMaybe $ filter ((==ipid).Packages.installedPackageId) allPkgs)
#endif

-- read all dependency data from the to-be-linked files
loadObjDeps :: [LinkedObj] -- ^ object files to link
            -> IO (Map (Package, Module) (Deps, DepsLocation), [LinkableUnit])
loadObjDeps objs = prepareLoadedDeps <$> mapM readDepsFile' objs

loadArchiveDeps :: GhcjsEnv
                -> [FilePath]
                -> IO (Map (Package, Module) (Deps, DepsLocation), [LinkableUnit])
loadArchiveDeps env archives = modifyMVar (linkerArchiveDeps env) $ \m ->
  case M.lookup archives' m of
    Just r  -> return (m, r)
    Nothing -> loadArchiveDeps' archives >>= \r -> return (M.insert archives' r m, r)
  where
     archives' = S.fromList archives

loadArchiveDeps' :: [FilePath]
                 -> IO (Map (Package, Module) (Deps, DepsLocation), [LinkableUnit])
loadArchiveDeps' archives = do
  archDeps <- forM archives $ \file ->
    Ar.withAllObjects file $ \modulename h _len -> (,ArchiveFile file) <$>
        hReadDeps (file ++ ':':moduleNameString modulename) h
  return (prepareLoadedDeps $ concat archDeps)

prepareLoadedDeps :: [(Deps, DepsLocation)]
                  -> (Map (Package, Module) (Deps, DepsLocation), [LinkableUnit])
prepareLoadedDeps deps =
  let req     = concatMap (requiredUnits . fst) deps
      depsMap = M.fromList $ map (\d -> ((depsPackage (fst d)
                                         ,depsModule (fst d)), d))
                                 deps
  in  (depsMap, req)

requiredUnits :: Deps -> [LinkableUnit]
requiredUnits d = map (depsPackage d, depsModule d,) (IS.toList $ depsRequired d)

-- read dependencies from an object that might have already been into memory
-- pulls in all Deps from an archive
readDepsFile' :: LinkedObj -> IO (Deps, DepsLocation)
readDepsFile' (ObjLoaded name bs) = pure . (,InMemory name bs) $
                                    readDeps name (BL.fromStrict bs)
readDepsFile' (ObjFile file)      =
  (,ObjectFile file) <$> readDepsFile file

generateBase :: FilePath -> Base -> IO ()
generateBase outDir b =
  BL.writeFile (outDir </> "out.base.symbs") (Compactor.renderBase b)
