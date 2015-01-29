{-# LANGUAGE PolyKinds #-}

import Data.Functor.Identity
import Control.Monad.RWS
import Control.Monad.Fix
import Control.Monad.Trans.Either




main :: IO ()
main = do print "main"
          let r1 = runIdentity $ runEitherT $ m1  0
              r2 = runIdentity $ runEitherT $ either (EitherT . return . Left) m2 r1
              r3 = runIdentity $ runEitherT $ either (EitherT . return . Left) m3 r2
          print r1
          print r2
          print r3





m1 :: Monad m => Int -> EitherT String m Int
m1 x = return (123+x)


m2 :: Monad m => Int -> EitherT String m Int
m2 = EitherT . return . Left . const "dupa"


m3 :: Monad m => Int -> EitherT String m Int
m3 x = return (456+x)