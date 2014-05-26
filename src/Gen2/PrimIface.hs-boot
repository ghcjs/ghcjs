module Gen2.PrimIface (ghcjsPrimIface, mkGhcjsPrimOpId) where

import Id
import HscTypes
import PrimOp

ghcjsPrimIface :: ModIface

mkGhcjsPrimOpId :: PrimOp -> Id

