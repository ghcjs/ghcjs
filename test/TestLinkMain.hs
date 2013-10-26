module Main where

{-
  We use incremental linking in the testsuite to speed up compilation of
  the test programs. This module imports TestLinkBase, the dependencies
  of which are included in the base bundle
 -}
import TestLinkBase

main = return ()
