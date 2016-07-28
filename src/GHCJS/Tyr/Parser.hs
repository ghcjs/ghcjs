{- 
  conversion utilities for language-ecmascript source
 -}
module GHCJS.Tyr.Parser ( {- parseExp, parseStat
                        , convertExp, convertStat -}
                        ) where

-- import qualified Language.ECMAScript3.Parser as Parser
import qualified GHCJS.Tyr.LeParser as Parser

import           Language.ECMAScript3.Syntax
import qualified Language.ECMAScript3.Syntax as ES
import           Language.ECMAScript3.Syntax.Annotations

import           Control.Arrow ((***))
import           Control.Lens

import           Data.Monoid
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T

import           Text.Parsec.Pos
import           Text.Parsec.Error

import           Panic

import           GHCJS.Tyr.Base
import qualified GHCJS.Tyr.Base as Tyr
import           GHCJS.Tyr.Number
import           GHCJS.Tyr.Operators
import           GHCJS.Tyr.Types
