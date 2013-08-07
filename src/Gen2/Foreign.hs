{-
  This module takes over desugaring and typechecking foreign declarations and calls
  from GHC. foreign import javascript should be desugared differently
  from other foreign imports since we don't want Bool to de be marshalled through
  0/1 for example.
 -}

module Gen2.Foreign where

import Hooks
import DynFlags

import Id
import OrdList
import Name
import Bag
import CoreSyn
import HscTypes
import HsBinds
import HsDecls
import DsForeign
import DsMonad
import TcRnTypes
import TcForeign
import MonadUtils
import RdrName

installForeignHooks :: Bool -> DynFlags -> DynFlags
installForeignHooks generatingJs df = df { hooks = f generatingJs (hooks df) }
  where
    f True  = insertHook DsForeignsHook       ghcjsDsForeigns
            . insertHook TcForeignImportsHook ghcjsTcForeignImports
            . insertHook TcForeignExportsHook ghcjsTcForeignExports
    f False = insertHook DsForeignsHook       ghcjsNativeDsForeigns
            . insertHook TcForeignImportsHook ghcjsTcForeignImports
            . insertHook TcForeignExportsHook ghcjsTcForeignExports

-- desugar foreign declarations for JavaScript
ghcjsDsForeigns :: [LForeignDecl Id]
                -> DsM (ForeignStubs, OrdList (Id, CoreExpr))
ghcjsDsForeigns fos = do
  liftIO $ putStrLn "desugaring foreign decls for JavaScript"
  dsForeigns' fos

-- desugar foreign declarations for native code
ghcjsNativeDsForeigns :: [LForeignDecl Id]
                      -> DsM (ForeignStubs, OrdList (Id, CoreExpr))
ghcjsNativeDsForeigns fos = do
  liftIO $ putStrLn "desugaring foreign decls for Native"
  dsForeigns' fos

ghcjsTcForeignImports :: [LForeignDecl Name]
                      -> TcM ([Id], [LForeignDecl Id], Bag GlobalRdrElt)
ghcjsTcForeignImports decls = do
  liftIO $ putStrLn "typechecking foreign imports"
  tcForeignImports' decls

ghcjsTcForeignExports :: [LForeignDecl Name]
                      -> TcM (LHsBinds TcId, [LForeignDecl TcId], Bag GlobalRdrElt)
ghcjsTcForeignExports decls = do
  liftIO $ putStrLn "typechecking foreign exports"
  tcForeignExports' decls


