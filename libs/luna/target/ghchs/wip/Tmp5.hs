{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

--{-# LANGUAGE ImpredicativeTypes #-}

module Main where

import Control.Monad.State
import Unsafe.Coerce

data X = X 
data Y = Y 

class Property a b | a -> b where
  property :: a -> b


newtype AnyMonad a = AnyMonad { pickMonad :: forall m. Monad m => m a } 

newtype AnyMonadS a = AnyMonadS { pickMonadS :: forall m s. (MonadState s m, Num s) => m a } 


instance Property X Y where
  property X = Y 


instance Property Y (AnyMonad Int) where
  property Y = AnyMonad (return 5)


-- Testing properties that work in arbitrary monads
instance Property a b => Property (AnyMonad a) b where
  property = property . head . pickMonad



-- The case that was an issue for type inference previously
twoProps :: (Property b c, Property a b) => a -> c
twoProps = property . property

--main :: IO ()
--main = print =<< pickMonad (twoProps X)




-- Possible extension

class    AsIO m        where io :: m a -> IO a
instance AsIO AnyMonad where io = pickMonad
instance AsIO IO       where io = id

newmain :: IO ()
newmain = print =<< io (twoProps X)

tst0 = AnyMonad $ return 5

coerceMonad :: m1 a -> (forall m. m a)
coerceMonad = unsafeCoerce


coerceDummy :: a -> Int
coerceDummy = unsafeCoerce

tst = AnyMonadS $ do
    x <- get
    put (x+1)

--main = print $ runState tst 0
data Xx = Xx

main = do
    let x = coerceDummy (Xx)
    print x

    print "end"