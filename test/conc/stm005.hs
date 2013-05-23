{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- GHC ticket #5238, by Mikhail Vorozhtsov

import Control.Exception
import Control.Concurrent.STM
import GHC.Conc (STM(..), ThreadId(..), myThreadId)
import GHC.Prim (getMaskingState#, killThread#)

getMaskingStateSTM = STM $ \s → case getMaskingState# s of
 (# s', i #) -> (# s', case i of 0# → Unmasked
                                 1# → MaskedUninterruptible
                                 _  → MaskedInterruptible #)

throwToSTM :: Exception e => ThreadId -> e -> STM ()
throwToSTM (ThreadId tid) ex = STM $ \s ->
   case (killThread# tid (toException ex) s) of s1 -> (# s1, () #)

main = do
  tid <- myThreadId
  mss ← atomically $ do
    ms1 ← getMaskingStateSTM
    (throwSTM Overflow) `catchSTM` (\(e ∷ SomeException) → return ())
    ms2 ← getMaskingStateSTM
    return (ms1, ms2)
  putStrLn $ show mss
  atomically $ do
    (throwToSTM tid Overflow) `catchSTM` (\(e :: SomeException) -> return ())
