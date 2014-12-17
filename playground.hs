{-# LANGUAGE PolyKinds #-}

import Control.Monad.RWS
import Control.Monad.Fix
import Control.Monad.Trans.Either

--tc :: (Monad m, Monoid w) => Int -> RWST r w s (EitherT e m) Int
tc :: Int -> RWST Int [Int] Int (EitherT String IO) Int
tc n | n < 100    = do
  st <- get
  re <- ask
  liftIO $ print $  "a : " ++ show n
  (a, w) <- listen (tc (n + st + re))
  tell $ (st + re):w
  return a
tc init = return init



main :: IO ()
main = do print "DUPA"
          res <- runEitherT $ runRWST (tc 0) 10 20
          print (show res)
