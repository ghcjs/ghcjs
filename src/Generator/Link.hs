{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Generator.Link (
    link
) where

import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import Data.List (isPrefixOf, intercalate, stripPrefix)
import Data.Char (isAlphaNum)
import Data.Tuple (swap)
import Data.Maybe (catMaybes)
import Data.Either (partitionEithers)
import Data.Monoid (Monoid)

import Prelude hiding(catch)
import System.FilePath ((</>), takeExtension, takeFileName)
import System.Directory (doesDirectoryExist, getDirectoryContents, copyFile, getModificationTime)
import Control.Applicative ((<$>))
import Control.Monad (forM, forM_, filterM)
import Control.Exception (catch, IOException)
import System.IO.Error (isDoesNotExistError)
import System.IO (openFile, hGetLine, hIsEOF, IOMode(..), Handle, hClose)
import Encoding (zEncodeString)
import Module (ModuleName, moduleNameString, mkModuleName, moduleNameSlashes)
import Distribution.Verbosity (Verbosity, normal)
import Distribution.Simple.Utils (findFileWithExtension, info, getDirectoryContentsRecursive, installOrdinaryFiles)

data DependencyInfo = DependencyInfo {
    modules      :: S.Set ModuleName
  , files        :: S.Set FilePath
  , toSearch     :: [ModuleName]
  , functionDeps :: [(FilePath, String, [String])]}

emptyDeps = DependencyInfo S.empty S.empty [] []
appendDeps a b = DependencyInfo
                    (modules a `S.union` modules b)
                    (files a `S.union` files b)
                    (toSearch a ++ toSearch b)
                    (functionDeps a ++ functionDeps b)

link :: String -> [FilePath] -> [FilePath] -> [String] -> [String] -> IO [String]
link out searchPath objFiles pageModules pageFunctions = do
    let pageModules' = map mkModuleName pageModules

    -- Copy all the .jso directories
    forM_ searchPath (\dir -> installJavaScriptObjects dir out)

    -- Read in the dependencies stored at the start of each .js file.
    -- Read the .js files that corrispond to .o files.  We need to load them to get the ModuleName.
    let maybeObjectDeps dep = case S.elems $ modules dep of
                                [mod] -> Just (mod, dep)
                                _     -> Nothing
    objDeps <- (M.fromList . catMaybes . map maybeObjectDeps) <$> mapM readDeps objFiles

    -- Search for the required modules in the packages
    let initDeps = emptyDeps{modules=S.singleton (mkModuleName "GHC.Prim"), toSearch=pageModules'}
    allDeps <- searchModules searchPath objDeps initDeps

    let deps = functionDeps allDeps

        -- Make a graph based on the dependencies.
        (graph, lookupEdges, lookupVertex) = G.graphFromEdges deps

        -- Make a set of all module prefix's.
        moduleSet = S.fromList $ map (("$$"++) . zEncodeString) pageModules

        -- Make a set of the functions in their encoded form.
        encodeFunction s = mod ++ "_" ++ (zEncodeString $ reverse rFunc)
            where (rFunc, rMod) = span (/='.') $ reverse s
                  mod   = "$$" ++ (zEncodeString $ reverse rMod)
        functionSet = S.fromList $ map encodeFunction pageFunctions

        -- Find all the functions that we want to make into "pages".
        page (_, symbol, _) | (takeWhile (/='_') symbol) `S.member` moduleSet = Just symbol
        page (_, symbol, _) | symbol `S.member` functionSet = Just symbol
        page _ = Nothing
        pages :: [G.Vertex]
        pages = catMaybes $ map (\dep -> page dep >>= lookupVertex) deps

        -- Used by all
        primatives = catMaybes $ map lookupVertex [
            "$$GHCziTypes_False"
          , "$$GHCziTypes_True"]

        -- For every function identify the "pages" that need it.
        pageMap :: G.Vertex -> M.Map G.Vertex (S.Set G.Vertex)
        pageMap page = M.fromList $ zip (G.reachable graph page ++ primatives) (repeat $ S.fromList [page])
        functionToPageSet = M.unionsWith S.union (map pageMap pages)

        -- Group functions based by the set of pages that use them.
        pageSetToFunctions = M.fromListWith (++) . map (\(f, ps) -> (ps, [f])) $ M.toList functionToPageSet

        -- Comment listing the pages in a page set.
        lookupKey = (\(_,k,_)->k) . lookupEdges
        pageSetComment pageSet = map (\page -> '/':'/':(lookupKey page)) pageSet

        fileAndKey (file, key, _) = (file, [key])

    -- Create Java Script for each page set.
    scripts <- forM (M.toList pageSetToFunctions) $ \(pageSet, functions) -> do
        script <- mapM makeScript . M.toList . M.fromListWith (++) $ map (fileAndKey . lookupEdges) functions
        return (sum $ map length script, (pageSet, script))

    -- Combine smaller page sets (based on the size of the script).
    let compareSize (a,_) (b,_) = compare a b
        scriptsBySize = L.sortBy compareSize scripts
        -- If the smallest two sizes is less than 20k then cobine them.
        combineSmall (a@(sa,(psA,scriptA)):b@(sb,(psB,scriptB)):rest) | sa + sb < 20000 =
            let new = (sa+sb, (psA `S.union` psB, scriptA++scriptB)) in
            combineSmall (L.insertBy compareSize new rest)
        combineSmall x = x
        combinedScripts = map snd $ combineSmall scriptsBySize

    closureMods <- forM (zip [1..] combinedScripts) $ \(n, (pageSet, script)) -> do
        writeFile (out++"hs"++show n++".js") . unlines $
            (pageSetComment $ S.toList pageSet) ++ script ++
            [("//@ sourceURL=hs"++show n++".js")]
        return (n, pageSet)

    let pageToCMod = M.fromListWith (++) $ concatMap (\(m, ps) -> map (\p -> (p, [m])) $ S.toList ps) closureMods

        loader = map makeLoader (M.toList pageToCMod)

        makeLoader (p, mods) = concat ["var $", lookupKey p, "=function() { $hs_load(",
            show mods, "); return ", lookupKey p, "; };"]

    writeFile (out++"hsloader.js") $ unlines loader

    return $ concatMap (\(n, _) -> [
                        "--js", concat [out, "hs", show n, ".js"],
                        "--module", concat ["hs", show n, "min:1:rts"]]
                        ) closureMods ++ [
                        "--js", concat [out, "hsloader.js"]]

-- | This installs all the java script (.js) files in a directory to a target loction
-- preserving the directory layout.  Any files in ".jsexe" directories are ignored
-- as those sube directoies are likely to be the destination.
--
-- Only files with newer modification times are copied.
--
installJavaScriptFiles :: Verbosity -> FilePath -> FilePath -> IO ()
installJavaScriptFiles verbosity srcDir destDir = do
    info verbosity $ "Copying Java Script From" ++ srcDir
    srcFiles <- getDirectoryContentsRecursive srcDir >>= filterM modTimeDiffers
    installOrdinaryFiles verbosity destDir [ (srcDir, f) | f <- srcFiles, takeExtension f == ".js" ]
  where
    modTimeDiffers f = do
            srcTime  <- getModificationTime $ srcDir </> f
            destTime <- getModificationTime $ destDir </> f
            return $ destTime < srcTime
        `catch` \e -> if isDoesNotExistError e
                            then return True
                            else ioError e

installJavaScriptObjects :: FilePath -> FilePath -> IO ()
installJavaScriptObjects dir out = do
    contents <- getDirectoryContents dir
    forM_ (filter ((/= '.') . head) contents) $ \item -> do
        let full = dir </> item
        isDir <- doesDirectoryExist full
        case (isDir, takeExtension item) of
            (True, ".jso") -> installJavaScriptFiles normal full (out </> item)
            _              -> return ()

searchModules :: [FilePath] -> M.Map ModuleName DependencyInfo -> DependencyInfo -> IO DependencyInfo
searchModules searchPath objDeps = loop
  where
    loop deps@DependencyInfo{toSearch=[]} = return deps -- No more modules to search
    loop deps@DependencyInfo{toSearch=(mod:mods)} = do
        case (mod `S.member` (modules deps), M.lookup mod objDeps) of
            (True, _)        -> loop deps{toSearch=mods} -- We already seearched this module
            (False, Just d)  -> loop (appendDeps deps d) -- It was in an object file
            (False, Nothing) -> do
                mbFile <- findFileWithExtension ["js"] searchPath (moduleNameSlashes mod)
                case mbFile of
                    Just file | not (file `S.member` (files deps)) -> do
                        fileDeps <- readDeps file
                        loop (appendDeps deps fileDeps)
                    _ -> loop deps{toSearch=mods} -- Can't find a file or we already seearched this file

readDeps :: FilePath -> IO DependencyInfo
readDeps file = do
    let fileDeps = emptyDeps{files = S.singleton file}
    h <- openFile file ReadMode
    eof <- hIsEOF h
    deps <- if eof
        then return fileDeps
        else do
            hGetLine h -- Skip blank line
            eof <- hIsEOF h
            if eof
                then return fileDeps
                else do
                    firstLine <- hGetLine h
                    case (stripPrefix "//GHCJS Haskell Module " firstLine) of
                        (Just s) -> do
                            case readOne s of
                                Nothing -> return fileDeps
                                Just (mod, toSearch) -> do
                                    functionDeps <- loop h []
                                    return fileDeps{
                                        modules   = S.singleton $ mkModuleName mod,
                                        toSearch  = map mkModuleName $ filter (not . (":" `isPrefixOf`)) toSearch,
                                        functionDeps}
                        _ -> return fileDeps
    hClose h
    return deps
  where
    loop h x = do
        eof <- hIsEOF h
        if eof
            then return x
            else do
                line <- hGetLine h
                case line of
                    '/':'/':s -> loop h (readDep s x)
                    _         -> return x
    readDep s x = case reads s of
                    (((a, b), ""):_) -> (file, a, b):x
                    _                -> x
    readOne s = case reads s of
                    ((x, ""):_) -> Just x
                    _           -> Nothing


makeScript :: (FilePath, [String]) -> IO String
makeScript (_, []) = return ""
makeScript ("", _) = return ""
makeScript (file, symbols) = do
    file <- openFile file ReadMode
    contents <- readContents False [] file
    hClose file
    return $ unlines contents
  where
    readContents includeFunction rContents file = do
        eof <- hIsEOF file
        if eof
            then return (reverse rContents)
            else do
                line <- hGetLine file
                case (includeFunction, line) of
                    (_, ('v':'a':'r':' ':'$':'$':_)) -> do
                        case span (\c -> isAlphaNum c || c == '$' || c == '_') (drop 4 line) of
                            (s, '=':_) | filterBySymb s -> readContents True (line:rContents) file
                            _                           -> readContents False rContents file
                    (True, _) -> readContents True (line:rContents) file
                    _         -> readContents False rContents file

    symbolSet = S.fromList symbols

    filterBySymb :: String -> Bool
    filterBySymb = flip S.member symbolSet

