{-# LANGUAGE TypeOperators #-}


module Data where

import Control.Applicative

data Pure a = Pure { fromPure :: a } deriving (Show)

instance Monad Pure where
    return = Pure
    (Pure a) >>= f = f a

instance Functor Pure where
    fmap f (Pure a) = Pure (f a)

instance Applicative Pure where
    pure = Pure
    (Pure f) <*> (Pure a) = Pure $ f a



newtype a :> m = InContext (m a) deriving Show

ctxmap f (InContext a) = InContext $ fmap f a


liftCtx                 = InContext
unliftCtx (InContext a) = a


ctxPure :: a -> a :> Pure
ctxPure = liftCtx . Pure

ctxIO :: a -> a :> IO
ctxIO = liftCtx . return