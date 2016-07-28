module GHCJS.Tyr.SourceMap where

import GHCJS.Tyr.Base
import GHCJS.Tyr.Types

import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.Maybe
import Data.Text (unpack)

import qualified Data.Aeson.Encode as Aeson
import qualified SourceMap         as S
import qualified SourceMap.Types   as S

makeSourceMap :: FilePath -> [(L, Span)] -> ByteString
makeSourceMap file xs = Aeson.encode . S.generate $
  (S.SourceMapping file Nothing $ mapMaybe convertMapping xs)
  where
    i32 :: Int -> Int32
    i32 = fromIntegral
    convertMapping :: (L, Span) -> Maybe S.Mapping
    convertMapping (LocFile file fl fc _ _, Span sl sc _ _) =
      Just (S.Mapping (S.Pos (i32 sl) (i32 sc))
                      (Just $ S.Pos (i32 fl) (i32 fc))
                      (Just $ unpack file)
                      Nothing
           )
    convertMapping _ = Nothing
