{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Bind2 where 

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


instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s
        

get     = StateT $ \s -> return (s, s)
put s   = StateT $ \_ -> return ((), s)
state' f = StateT (return . f)

------------------------------------------------------------------------------------------


class (Monad m1, Monad m2) => MonadRebase t m1 m2 where
    rebase :: t m1 a -> t m2 a


instance MonadRebase (StateT a) Pure Pure where
    rebase = id

instance MonadRebase (StateT a) IO IO where
    rebase = id

instance MonadRebase (StateT a) Pure IO where
    rebase t = StateT $ \s -> return . fromPure $ runStateT t s


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

---

instance (Monad (m1 Pure), m1~m2) => Bind (m1 Pure) (m2 Pure) (m1 Pure) where
    bind ma f = do
        a <- ma 
        f (Pure a)

instance (Monad (m1 IO), m1~m2) => Bind (m1 IO) (m2 IO) (m1 IO) where
    bind ma f = do
        a <- ma 
        f (Pure a)

instance (Monad (m1 IO), MonadRebase m1 Pure IO, m1~m2) => Bind (m1 Pure) (m2 IO) (m1 IO) where
    bind ma f = do
        a <- rebase ma 
        f (Pure a)

instance (Monad (m1 IO), MonadRebase m1 Pure IO, m1~m2) => Bind (m1 IO) (m2 Pure) (m1 IO) where
    bind ma f = do
        a <- ma 
        rebase $ f (Pure a)

---

instance Bind Pure (m Pure) (m Pure) where
    bind a f = f a


instance Bind Pure (m IO) (m IO) where
    bind a f = f a


instance (Monad (m IO), MonadTrans m, MonadRebase m Pure IO) => Bind IO (m Pure) (m IO) where
    bind ma f = do
        a <- lift ma
        rebase $ f (Pure a)


instance (Monad (m IO), MonadTrans m) => Bind IO (m IO) (m IO) where
    bind ma f = do
        a <- lift ma
        f (Pure a)

---

instance Monad (m Pure) => Bind (m Pure) Pure (m Pure) where
    bind ma f = do
        a <- ma
        let Pure b = f (Pure a)
        return b


instance (Monad (m IO), MonadTrans m, MonadRebase m Pure IO) => Bind (m Pure) IO (m IO) where
    bind ma f = do
        a <- rebase ma
        lift $ f (Pure a)


instance Monad (m IO) => Bind (m IO) Pure (m IO) where
    bind ma f = do
        a <- ma
        let Pure b = f (Pure a)
        return b

instance (Monad (m IO), MonadTrans m) => Bind (m IO) IO (m IO) where
    bind ma f = do
        a <- ma
        lift $ f (Pure a)

--------------------------------------

class Bind2 m1 m2 m3 | m1 m2 -> m3 where
    bind2 :: a :> m1 -> (a :> Pure -> b :> m2) -> b :> m3

instance Bind2 Pure Pure Pure where
    bind2 a f = f a

instance Bind2 IO Pure IO where
    bind2 ma f = liftCtx $ do
        a <- unliftCtx ma
        let Pure b = unliftCtx $ f (liftCtx $ Pure a)
        return b

instance Bind2 Pure IO IO where
    bind2 a f = f a

instance Bind2 IO IO IO where
    bind2 ma f = liftCtx $ do
        a <- unliftCtx ma
        unliftCtx $ f (liftCtx $ Pure a)

---

instance (Monad (m1 Pure), m1~m2) => Bind2 (m1 Pure) (m2 Pure) (m1 Pure) where
    bind2 ma f = liftCtx $ do
        a <- unliftCtx ma 
        unliftCtx $ f (liftCtx $ Pure a)

instance (Monad (m1 IO), m1~m2) => Bind2 (m1 IO) (m2 IO) (m1 IO) where
    bind2 ma f = liftCtx $ do
        a <- unliftCtx ma 
        unliftCtx $ f (liftCtx $ Pure a)

instance (Monad (m1 IO), MonadRebase m1 Pure IO, m1~m2) => Bind2 (m1 Pure) (m2 IO) (m1 IO) where
    bind2 ma f = liftCtx $ do
        a <- rebase $ unliftCtx ma 
        unliftCtx $ f (liftCtx $ Pure a)

instance (Monad (m1 IO), MonadRebase m1 Pure IO, m1~m2) => Bind2 (m1 IO) (m2 Pure) (m1 IO) where
    bind2 ma f = liftCtx $ do
        a <- unliftCtx ma 
        rebase . unliftCtx $ f (liftCtx $ Pure a)

---

instance Bind2 Pure (m Pure) (m Pure) where
    bind2 a f = f a


instance Bind2 Pure (m IO) (m IO) where
    bind2 a f = f a


instance (Monad (m IO), MonadTrans m, MonadRebase m Pure IO) => Bind2 IO (m Pure) (m IO) where
    bind2 ma f = liftCtx $ do
        a <- lift . unliftCtx $ ma
        rebase . unliftCtx $ f (liftCtx $ Pure a)


instance (Monad (m IO), MonadTrans m) => Bind2 IO (m IO) (m IO) where
    bind2 ma f = liftCtx $ do
        a <- lift . unliftCtx $ ma
        unliftCtx $ f (liftCtx $ Pure a)

---

instance Monad (m Pure) => Bind2 (m Pure) Pure (m Pure) where
    bind2 ma f = liftCtx $ do
        a <- unliftCtx ma
        let Pure b = unliftCtx $ f (liftCtx $ Pure a)
        return b


instance (Monad (m IO), MonadTrans m, MonadRebase m Pure IO) => Bind2 (m Pure) IO (m IO) where
    bind2 ma f = liftCtx $ do
        a <- rebase . unliftCtx $ ma
        lift . unliftCtx $ f (liftCtx $ Pure a)


instance Monad (m IO) => Bind2 (m IO) Pure (m IO) where
    bind2 ma f = liftCtx $ do
        a <- unliftCtx ma
        let Pure b = unliftCtx $ f (liftCtx $ Pure a)
        return b

instance (Monad (m IO), MonadTrans m) => Bind2 (m IO) IO (m IO) where
    bind2 ma f = liftCtx $ do
        a <- unliftCtx $ ma
        lift . unliftCtx $ f (liftCtx $ Pure a)

------------------------------------------------------------------------------------------

testIO :: IO Int
testIO = do
    liftIO $ print "dupa jeza"
    return 5

testStateT :: StateT Int IO Int
testStateT = do
    liftIO $ print "dupa jeza"
    x <- get
    put(x+1)
    return 5


testStateT2 :: Pure Int -> StateT Int Pure Int
testStateT2 (Pure x) = do
    return (x*2)


testPure :: Int :> Pure
testPure = liftCtx $ return 5


printCtx :: Show s => s :> Pure -> () :> IO
printCtx s = liftCtx $ print (fromPure $ unliftCtx s)


bind_ a b = bind a (\_ -> b)


main = do 
    --let x = testStateT `bind` testStateT2
    let x = testIO `bind` testStateT2
    print =<< runStateT x 5

    let y = testPure `bind2` printCtx
    unliftCtx y

    --let y = testStateT
    --    z = rebase y :: StateT Int IO Int
    print "end"


