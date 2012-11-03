{-
  The Javascript dependencies of the runtime system
-}
module RTS.Dependencies where

import System.FilePath ((</>))

import Compiler.Variants
import Closure.Paths
import Paths_ghcjs
import qualified Paths_ghcjs_hterm as HT (getDataFileName)

-- | the absolute filepaths of the dependencies for the RTS
rtsDeps :: Variant -> IO [FilePath]
rtsDeps v = do
  rtsPath <- getDataFileName "rts"
  closurePath <- closureLibraryPath
  htermPath <- HT.getDataFileName "js"
  return $ map (closurePath </>) closureDeps
        ++ map (htermPath </>) htermDeps
        ++ map (rtsPath </>) (rtsSrcs v)

-- | the file that defines the default options for the RTS
rtsDefaultOptions  :: IO FilePath
rtsDefaultOptions = getDataFileName "rts/rts-options.js"

rtsSrcs :: Variant -> [FilePath]
rtsSrcs v =
    case variantCallingConvention v of
      Plain      -> [ "rts-common.js", "rts-webkit.js", "rts-plain.js" ]
      Trampoline -> [ "rts-common.js", "rts-webkit.js", "rts-trampoline.js" ]
      Gen2      -> []

closureDeps :: [FilePath]
closureDeps =
   [ "closure/goog/base.js"
   , "closure/goog/object/object.js"
   , "closure/goog/string/string.js"
   , "closure/goog/debug/error.js"
   , "closure/goog/asserts/asserts.js"
   , "closure/goog/array/array.js"
   , "closure/goog/math/long.js"
   , "closure/goog/math/integer.js"
   , "closure/goog/debug/relativetimeprovider.js"
   , "closure/goog/debug/formatter.js"
   , "closure/goog/structs/structs.js"
   , "closure/goog/structs/collection.js"
   , "closure/goog/iter/iter.js"
   , "closure/goog/structs/map.js"
   , "closure/goog/structs/set.js"
   , "closure/goog/useragent/useragent.js"
   , "closure/goog/debug/debug.js"
   , "closure/goog/debug/logrecord.js"
   , "closure/goog/debug/logbuffer.js"
   , "closure/goog/debug/logger.js"
   , "closure/goog/debug/console.js"
   , "closure/goog/crypt/hash.js"
   , "closure/goog/crypt/md5.js"
   ]

htermDeps :: [FilePath]
htermDeps =
   [ "lib.js"
   , "lib_f.js"
   , "lib_colors.js"
   , "lib_fs.js"
   , "lib_message_manager.js"
   , "lib_preference_manager.js"
   , "lib_test_manager.js"
   , "lib_utf8.js"
   , "hterm_frame.js"
   , "hterm.js"
   , "hterm_keyboard.js"
   , "hterm_keyboard_keymap.js"
   , "hterm_mock_row_provider.js"
   , "hterm_options.js"
   , "hterm_pubsub.js"
   , "hterm_screen.js"
   , "hterm_scrollport.js"
   , "hterm_terminal_io.js"
   , "hterm_terminal.js"
   , "hterm_text_attributes.js"
   , "hterm_vt_character_map.js"
   , "hterm_vt.js"
   ]
