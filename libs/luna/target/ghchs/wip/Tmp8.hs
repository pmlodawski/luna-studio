{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

--{-# LANGUAGE ImpredicativeTypes #-}

{-# LANGUAGE DysfunctionalDependencies #-}


module Main where

import Control.Monad.State
import Unsafe.Coerce

data X = X
data Y = Y

class Property a b | a -> b where
  property :: a -> b


newtype AnyMonad a = AnyMonad { fromAnyMonad :: forall m. Monad m => m a }

newtype AnyMonadS a = AnyMonadS { pickMonadS :: forall m s. (MonadState s m, Num s) => m a }



instance Property X Y where
  property X = Y


instance Property Y (AnyMonad Int) where
  property Y = AnyMonad (return 5)


-- Testing properties that work in arbitrary monads
instance Property a b => Property (AnyMonad a) b where
  property = property . head . fromAnyMonad



-- The case that was an issue for type inference previously
twoProps :: (Property b c, Property a b) => a -> c
twoProps = property . property

--main :: IO ()
--main = print =<< fromAnyMonad (twoProps X)




-- Possible extension

class    AsIO m        where io :: m a -> IO a
instance AsIO AnyMonad where io = fromAnyMonad
instance AsIO IO       where io = id

newmain :: IO ()
newmain = print =<< io (twoProps X)

tst0 = AnyMonad $ return 5

coerceMonad :: m1 a -> (forall m. m a)
coerceMonad = unsafeCoerce


coerceDummy :: a -> Int
coerceDummy = unsafeCoerce

tst = do
    x <- get
    put (x+1)
    --AnyMonad $ return 5

--tst2 = do
--    AnyMonad $ get
--    return 4

--main = print $ runState tst 0
data Xx = Xx

--instance MonadState s AnyMonad where


instance Monad AnyMonad where
    return a = AnyMonad $ return a
    AnyMonad ma >>= f = AnyMonad $ do
        a <- ma
        fromAnyMonad $ f a


main = do
    let x = coerceDummy (Xx)
    print x

    print "end"
