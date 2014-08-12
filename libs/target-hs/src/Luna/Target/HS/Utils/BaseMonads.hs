---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}


!{-# LANGUAGE RightSideContexts #-}


module Luna.Target.HS.Utils.BaseMonads where

import Luna.Target.HS.Control.Error
import Luna.Target.HS.Control.Flow.Utils
import Control.Monad.Trans
import Control.Monad.Morph
import Data.Typeable (Typeable)
import Luna.Target.HS.Control.Context
import Data.TypeLevel
import Data.Typeable
import Control.Applicative
import Control.Monad (ap)

--print' = liftIO . print


instance MonadIO (t m) <= (MonadIO m, MonadTrans t, Monad (t m)) where
    liftIO = lift . liftIO

--------------------------------------------------------------------------------
-- ReaderT monad
--------------------------------------------------------------------------------


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a } deriving (Typeable)

class Monad m => MonadReader r m | m -> r where
    {-# MINIMAL (ask | reader), local #-}
    ask   :: m r
    ask = reader id

    local :: (r -> r) -> m a -> m a

    reader :: (r -> a) -> m a
    reader f = do
      r <- ask
      return (f r)

withReaderT f m = ReaderT $ runReaderT m . f

instance MonadTrans (ReaderT r) where
    lift m = ReaderT (const m)

instance (Monad m) => Monad (ReaderT r m) where
    return   = lift . return
    m >>= k  = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    fail msg = lift (fail msg)

instance Monad m => MonadReader r (ReaderT r m) where
    ask = ReaderT return
    local = withReaderT
    reader f = ReaderT (return . f)

instance MFunctor (ReaderT r) where
    hoist nat m = ReaderT (\i -> nat (runReaderT m i))

instance (Monad m) => Functor (ReaderT r m) where
    fmap f m = ReaderT $ \r -> do
        a <- runReaderT m r
        return (f a)


--------------------------------------------------------------------------------
-- StateT monad
--------------------------------------------------------------------------------

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) } deriving (Typeable)

instance (Monad m) => Monad (StateT s m) where
    return a = state $ \s -> (a, s)
    m >>= k  = StateT $ \s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \_ -> fail str

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (a, s)


instance (Functor m) => Functor (StateT s m) where
    fmap f m = StateT $ \ s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runStateT m s


instance (MonadReader r m) => MonadReader r (StateT s m) where
    ask       = lift ask
    local f m = StateT $ \s -> local f (runStateT m s)


class (Monad m) => MonadState s m | m -> s where
    get :: m s
    put :: s -> m (Value Pure (Safe ()))

instance MonadState s (StateT s m) <= Monad m where
    get   = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return (val (), s)

instance MonadState s (ReaderT s m) <= MonadState s m where
    get   = lift $ get
    put s = lift $ put s

instance MonadState s (t s m) <= (MonadTrans (t s), MonadState s m, Monad (t s m)) where
    get   = lift $ get
    put s = lift $ put s

instance MFunctor (StateT s) where
    hoist nat m = StateT (\s -> nat (runStateT m s))

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure = return
    (<*>) = ap

state f = StateT (return . f)


----------------------------------------------------------------------

getX :: MonadCtx Pure (Insert (Proxy StateT) Empty) m s <= MonadState s m
getX = MonadCtx get

putX :: s -> MonadCtx Pure (Insert (Proxy StateT) Empty) m (Value Pure (Safe ())) <= MonadState s m
putX = MonadCtx . put

askX :: MonadCtx Pure (Insert (Proxy ReaderT) Empty) m s <= MonadReader s m
askX = MonadCtx ask

--getX' :: MonadCtx Pure (ConstrainSet () (Insert (Proxy StateT) Empty)) m s <= MonadState s m
--getX' = MonadCtx get

runStateTX  = liftMonadRunner1 (Proxy :: Proxy StateT)  runStateT  . appMonadCtx
runReaderTX = liftMonadRunner1 (Proxy :: Proxy ReaderT) runReaderT . appMonadCtx

--runStateTX''  = liftMonadRunner1'' (Proxy :: Proxy StateT)  runStateT
--runReaderTX'' = liftMonadRunner1'' (Proxy :: Proxy ReaderT) runReaderT


--liftMonadRunner1' :: MatchMonadCloseProto (IsEmpty (Remove mptr set)) (MonadCtx env (Remove mptr set) mb) t => mptr -> (ma a1 -> a -> mb b) -> MonadCtx env set ma a1 -> a -> t b
--liftMonadRunner1' (mf :: (t x (ma :: * -> *) a1 -> a -> mb b)) = liftMonadRunner1 (Proxy :: Proxy t) mf


