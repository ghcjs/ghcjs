module GHCJS.Tyr.Quasi where

import Language.Haskell.TH.Quote

-- quasiquoter for Tyr

j :: QuasiQuoter
j = QuasiQuoter
      { quoteExp  = qTyrStat
      , quotePat  = error "Gen2.Tir.Quasi.j: quotePat"
      , quoteDec  = error "Gen2.Tir.Quasi.j: quoteDec"
      , quoteType = error "Gen2.Tir.Quasi.j: quoteType"
      }

je :: QuasiQuoter
je = QuasiQuoter
       { quoteExp  = qTyrExp
       , quotePat  = error "Gen2.Tir.Quasi.je: quotePat"
       , quoteDec  = error "Gen2.Tir.Quasi.je: quoteDec"
       , quoteType = error "Gen2.Tir.Quasi.je: quoteType"
       }

qTyrExp = undefined

qTyrStat = undefined
