{-# LANGUAGE ScopedTypeVariables #-}
module Generator.Link (
    link
) where

import qualified Data.Graph as G
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L

import Data.List (isPrefixOf, intercalate)
import Data.Char (isAlphaNum)
import Data.Tuple (swap)
import Data.Maybe (catMaybes)

import Prelude hiding(catch)
import System.FilePath ((</>), takeExtension)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import Control.Applicative ((<$>))
import Control.Monad (forM, forM_)
import Control.Exception (catch, IOException)
import System.IO (openFile, hGetLine, IOMode(..), Handle)
import Encoding (zEncodeString)

link out dirs pageModules pageFunctions = do
    -- Find all the .js files
    files <- concat <$> forM dirs  findJavascript

    -- Read in the dependencies stored at the start of each .js file
    deps  <- concat <$> forM files readDeps

    let -- Make a graph based on the dependencies
        (graph, lookupEdges, lookupVertex) = G.graphFromEdges deps

        -- Make a set of all module prefix's
        moduleSet = S.fromList $ map (("$$"++).zEncodeString) pageModules

        -- Make a set of the functions in their encoded form
        encodeFunction s = mod ++ "_" ++ (zEncodeString $ reverse rFunc)
            where (rFunc, rMod) = span (/='.') $ reverse s
                  mod   = "$$" ++ (zEncodeString $ reverse rMod)
        functionSet = S.fromList $ map encodeFunction pageFunctions

        -- Find all the functions that we want to make into "pages"
        page (_, symbol, _) | (takeWhile (/='_') symbol) `S.member` moduleSet = Just symbol
        page (_, symbol, _) | symbol `S.member` functionSet = Just symbol
        page _ = Nothing
        pages :: [G.Vertex]
        pages = catMaybes $ map (\dep -> page dep >>= lookupVertex) deps

        -- For every function identify the "pages" that need it
        pageMap :: G.Vertex -> M.Map G.Vertex (S.Set G.Vertex)
        pageMap page = M.fromList $ zip (G.reachable graph page) (repeat $ S.fromList [page])
        functionToPageSet = M.unionsWith S.union (map pageMap pages)

        -- Group functions based by the set of pages that use them
        pageSetToFunctions = M.fromListWith (++) . map (\(f, ps) -> (ps, [f])) $ M.toList functionToPageSet

        -- Comment listing the pages in a page set
        lookupKey = (\(_,k,_)->k) . lookupEdges
        pageSetComment pageSet = map (\page -> '/':'/':(lookupKey page)) pageSet

        fileAndKey (file, key, _) = (file, [key])

    -- Create Java Script for each page set
    scripts <- forM (M.toList pageSetToFunctions) $ \(pageSet, functions) -> do
        script <- mapM makeScript . M.toList . M.fromListWith (++) $ map (fileAndKey . lookupEdges) functions
        return (sum $ map length script, (pageSet, script))

    -- Combine smaller page sets (based on the size of the script)
    let compareSize (a,_) (b,_) = compare a b
        scriptsBySize = L.sortBy compareSize scripts
        -- If the smallest two sizes is less than 20k then cobine them
        combineSmall (a@(sa,(psA,scriptA)):b@(sb,(psB,scriptB)):rest) | sa + sb < 20000 =
            let new = (sa+sb, (psA `S.union` psB, scriptA++scriptB)) in
            combineSmall (L.insertBy compareSize new rest)
        combineSmall x = x
        combinedScripts = map snd $ combineSmall scriptsBySize

    closureMods <- forM (zip [1..] combinedScripts) $ \(n, (pageSet, script)) -> do
        writeFile (out++"hs"++show n++".js") . unlines $ (pageSetComment $ S.toList pageSet) ++ script
        return (n, pageSet)

    let pageToCMod = M.fromListWith (++) $ concatMap (\(m, ps) -> map (\p -> (p, [m])) $ S.toList ps) closureMods

        loader = map makeLoader (M.toList pageToCMod)

        makeLoader (p, mods) = concat ["var $", lookupKey p, "=function() { $hs.load(",
            show mods, "); return ", lookupKey p, "; };"]

    writeFile (out++"hsloader.js") $ unlines loader

    return $ concatMap (\(n, _) -> [
                        "--js", concat [out, "hs", show n, ".js"],
                        "--module", concat ["hs", show n, "min:1:rts"]]
                        ) closureMods ++ [
                        "--js", concat [out, "hsloader.js"]]

findJavascript :: FilePath -> IO [FilePath]
findJavascript dir = do
    contents <- getDirectoryContents dir
    concat <$> forM (filter ((/= '.') . head) contents) (\item -> do
        let full = dir </> item
        isDir <- doesDirectoryExist full
        case (isDir, takeExtension item) of
            (True, _)  -> findJavascript full
            (_, ".js") -> return [full]
            _          -> return [])

readDeps :: FilePath -> IO [(FilePath, String, [String])]
readDeps file = do
    h <- openFile file ReadMode
    hGetLine h `catch` (\(_::IOException) -> return [])
    loop h []
  where
    loop h x = do
        line <- hGetLine h `catch` (\(_::IOException) -> return [])
        case line of
            '/':'/':s -> loop h ((readDep s):x)
            _         -> return x
    readDep s = let (a, b) = read s in (file, a, b)

makeScript :: (FilePath, [String]) -> IO String
makeScript (_, []) = return ""
makeScript ("", _) = return ""
makeScript (file, symbols) = do
    contents <- filter includeLine . lines <$> readFile file
    return . unlines . concatMap snd . filter (filterBySymb . fst) $ parse ("",[]) contents
  where
    includeLine ""          = False
    includeLine ('/':'/':_) = False
    includeLine _           = True

    parse current@(cSymb, cLines) (line@('v':'a':'r':' ':'$':'$':_):rest) =
        case span (\c -> isAlphaNum c || c == '$' || c == '_') (drop 4 line) of
            (s, '=':_) -> current : parse (s, [line]) rest
            _          -> parse (cSymb, cLines++[line]) rest
    parse (cSymb, cLines) (line:rest) = parse (cSymb, cLines++[line]) rest
    parse current [] = [current]

    symbolSet = S.fromList symbols

    filterBySymb :: String -> Bool
    filterBySymb = flip S.member symbolSet

