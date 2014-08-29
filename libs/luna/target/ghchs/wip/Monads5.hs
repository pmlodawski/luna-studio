{-# LANGUAGE NoMonomorphismRestriction #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-} 

{-# LANGUAGE OverlappingInstances #-} 
{-# LANGUAGE UndecidableInstances #-} 
--{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE DysfunctionalDependencies #-} 

{-# LANGUAGE RankNTypes #-} 


!{-# LANGUAGE RightSideContexts #-}

import Control.Monad.Trans
import Control.Applicative
import Data.Functor.Identity (Identity)

import GHC.TypeLits        (Symbol)

import Luna.Target.HS.Data

data Proxy a = Proxy

print' = liftIO . print


instance MonadIO (t m) <= (MonadIO m, MonadTrans t, Monad (t m)) where
    liftIO = lift . liftIO

--------------------------------------------------------------------------------
-- ReaderT monad
--------------------------------------------------------------------------------


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

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


--------------------------------------------------------------------------------
-- StateT monad
--------------------------------------------------------------------------------

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

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
    put :: s -> m ()

instance MonadState s (StateT s m) <= Monad m where
    get   = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)

instance MonadState s (ReaderT s m) <= MonadState s m where
    get   = lift $ get
    put s = lift $ put s

instance MonadState s (t s m) <= (MonadTrans (t s), MonadState s m, Monad (t s m)) where
    get   = lift $ get
    put s = lift $ put s


state f = StateT (return . f)



--------------------------------------------------------------------------------
-- Call0
--------------------------------------------------------------------------------


class Call0 (name :: Symbol) base out | base name -> out where
    call0 :: Proxy name -> base -> out


instance Call0 "get" X1 (m a) <= MonadState a m where
    call0 _ _ = get


instance Call0 "ask" X2 (m a) <= MonadReader a m where
    call0 _ _ = ask


instance Call0 "test" V (m a) <= (MonadState a m, MonadReader t m, MonadIO m) where
    call0 _ _ = test


--------------------------------------------------------------------------------
-- utils
--------------------------------------------------------------------------------

testM2 = do
    x <- get
    y <- ask
    print' "hello"
    put (x+1)

data X1 = X1 deriving Show
data X2 = X2 deriving Show
data V = V deriving Show

test = do
    x <- call0 (Proxy :: Proxy "get") X1
    y <- call0 (Proxy :: Proxy "ask") X2
    print' "hello"
    return x


main = do
    print =<< runReaderT (runStateT test 0) 0
    print =<< runStateT (runReaderT test 0) 0

    print =<< runReaderT (runStateT (call0 (Proxy :: Proxy "test") V) 0) 0 


    putStrLn ""
    putStrLn "-----"
    putStrLn ""

