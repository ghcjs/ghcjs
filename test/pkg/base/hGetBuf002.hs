import System.IO
import Foreign
import Foreign.C
import Data.IORef

main = do test2
       --   test False

test2 = do
  r <- newIORef 3
  let loop = do
        b <- allocaBytes 10 $ \ptr -> do
--           putStrLn "loop"
--           hFlush stdout
           modifyIORef r (subtract 1) 
           readIORef r
        if (b > 0)
          then loop
          else return ()
  loop
{-
test3 = do
  let loop n = do
        b <- allocaBytes 10 $ \ptr -> do
          return (n-1)
        if(b > 0)
          then loop b
          else return ()
  loop (3::Int)
-}
{-
test = do
  h <- openBinaryFile "hGetBuf002.hs" ReadMode

  let sz = 42
      loop = do
  	 b <- allocaBytes sz $ \ptr -> do
                r <- hGetBuf h ptr sz
                print r
                hFlush stdout
                return (r > 0)
--                r `seq` return False 
--         	if (r == 0)
--		   then return True
--                   else return False
--	 if b then return () else loop -- tail call
         case b of
           True  -> loop -- error "errrr"
           False -> return ()
  loop

{-# NOINLINE hGetBuf2 #-}
hGetBuf2 x y z = do
  return (0::Int)

-}

