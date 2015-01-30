{-# LANGUAGE FlexibleContexts #-}

import Data.Functor.Identity
import Control.Monad.RWS
import Control.Monad.Fix
import Control.Monad.Trans.Either
import Control.Monad.State.Strict




main :: IO ()
main = do
    let a = runIdentity $ runEitherT (runStateT procedureA 0)
    let b = runIdentity $ runStateT (runEitherT procedureB) 0
    print $ "A = " ++ show a
    print $ "B = " ++ show b


procedureA :: StateT Int (EitherT String Identity) ()
procedureA = do
    x <- get
    r1 <- m1 x
    put r1
    r2 <- m2 r1
    put r2
    r3 <- m3 r2
    put r3
    return ()


procedureB :: EitherT String (StateT Int Identity) ()
procedureB = do
    x <- get
    r1 <- m1 x
    put r1
    r2 <- m2b r1
    put r2
    r3 <- m3 r2
    put r3
    return ()


m1 x = return (10 + x)

m2 x = return (100 + x)

m2b x = EitherT $ return $ Left "no"

m3 x = return (1000 + x)
