{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Bind where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans
--import Control.Monad.State

import Data



newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

instance (Monad m) => Monad (StateT s m) where
    return a = state' $ \s -> (a, s)
    m >>= k  = StateT $ \s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \_ -> fail str

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift . liftIO


get'     = StateT $ \s -> return (s, s)
put' s   = StateT $ \_ -> return ((), s)
state' f = StateT (return . f)

------------------------------------------------------------------------------------------


class (Monad m1, Monad m2) => MonadTransBase t m1 m2 where
    transBase :: t m1 a -> t m2 a


instance MonadTransBase (StateT a) Pure Pure where
    transBase = id

instance MonadTransBase (StateT a) IO IO where
    transBase = id

instance MonadTransBase (StateT a) Pure IO where
    transBase t = StateT $ \s -> return . fromPure $ runStateT t s


------------------------------------------------------------------------------------------


class Bind m1 m2 m3 | m1 m2 -> m3 where
    bind :: m1 a -> (Pure a -> m2 b) -> m3 b

instance Bind Pure Pure Pure where
    bind a f = f a

instance Bind IO Pure IO where
    bind ma f = do
        a <- ma
        let Pure b = f (Pure a)
        return b

instance Bind Pure IO IO where
    bind a f = f a

instance Bind IO IO IO where
    bind ma f = do
        a <- ma
        f (Pure a)


instance Monad (m Pure) => Bind (m Pure) (m Pure) (m Pure) where
    bind ma f = do
        a <- ma
        f (Pure a)

instance Monad (m IO) => Bind (m IO) (m IO) (m IO) where
    bind ma f = do
        a <- ma
        f (Pure a)

instance (Monad (m IO), MonadTransBase m Pure IO) => Bind (m Pure) (m IO) (m IO) where
    bind ma f = do
        a <- transBase ma
        f (Pure a)

instance (Monad (m IO), MonadTransBase m Pure IO) => Bind (m IO) (m Pure) (m IO) where
    bind ma f = do
        a <- ma
        transBase $ f (Pure a)


------------------------------------------------------------------------------------------



testStateT :: StateT Int IO Int
testStateT = do
    liftIO $ print "dupa jeza"
    x <- get'
    put'(x+1)
    return 5


testStateT2 :: Pure Int -> StateT Int Pure Int
testStateT2 (Pure x) = do
    return (x*2)


bind_ a b = bind a (\_ -> b)


main = do
    let x = testStateT `bind` testStateT2
    print =<< runStateT x 5

    --let y = testStateT
    --    z = transBase y :: StateT Int IO Int
    print "end"


