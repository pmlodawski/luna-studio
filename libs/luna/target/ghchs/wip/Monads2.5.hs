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


!{-# LANGUAGE RightSideContexts #-}

import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.Functor.Identity (Identity)

import GHC.TypeLits        (Symbol)

data Proxy a = Proxy

print' = liftIO . print



newtype StateT' s m a = StateT' { runStateT' :: s -> m (a,s) }

instance (Monad m) => Monad (StateT' s m) where
    return a = state' $ \s -> (a, s)
    m >>= k  = StateT' $ \s -> do
        ~(a, s') <- runStateT' m s
        runStateT' (k a) s'
    fail str = StateT' $ \_ -> fail str

instance MonadTrans (StateT' s) where
    lift m = StateT' $ \s -> do
        a <- m
        return (a, s)

instance (MonadIO m) => MonadIO (StateT' s m) where
    liftIO = lift . liftIO


instance (Functor m) => Functor (StateT' s m) where
    fmap f m = StateT' $ \ s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runStateT' m s


instance (MonadReader r m) => MonadReader r (StateT' s m) where
    ask       = lift ask
    local f m = undefined -- StateT' $ \s -> local f (runStateT' m s)
        

class (Monad m) => MonadState' s m | m -> s where
    get' :: m s
    put' :: s -> m ()

instance (Monad m) => MonadState' s (StateT' s m) where
    get'   = StateT' $ \s -> return (s, s)
    put' s = StateT' $ \_ -> return ((), s)

state' f = StateT' (return . f)

--instance (MonadIO m) => MonadIO (StateT s m) where
--    liftIO = lift . liftIO

testM2 = do
    x <- get'
    y <- ask
    print' "hello"
    put' (x+1)


testM2' = do
    x <- callM0 (Proxy :: Proxy "get") X1
    y <- callM0 (Proxy :: Proxy "ask") X2
    print' "hello"
    --put (x+1)

--class Call0 (name :: Symbol) base out | name base -> out where
--    call0 :: Proxy name -> base -> out


--instance MonadState c (StateT a b) => Call0 "get" (StateT a b c) (StateT a b c) where
--    call0 _ _ = get

data TestMonad a = TestMonad { runTestMonad :: a } deriving Show

instance Monad TestMonad where
    return = TestMonad
    (TestMonad a) >>= f = f a

instance Functor TestMonad where
    fmap f (TestMonad a) = TestMonad $ f a

instance Applicative TestMonad where
    pure = TestMonad
    (TestMonad f) <*> (TestMonad a) = TestMonad $ f a 


data X1 = X1 deriving Show
data X2 = X2 deriving Show


class CallM0 (name :: Symbol) base a m | m -> a where
    callM0 :: Proxy name -> base -> m a


instance CallM0 "get" X1 a (StateT' a m) <= Monad m where
    callM0 _ _ = StateT' $ \s -> return (s, s)

instance CallM0 "get" X1 a (t m) <= (Monad m, MonadTrans t, CallM0 "get" X1 a m) where
    callM0 base a = lift $ callM0 base a

--instance CallM0 "ask" X2 a (StateT' a m) <= (Monad m, CallM0 "ask" X2 a m) where
--    callM0 base a = lift $ callM0 base a

instance CallM0 "ask" X2 a (t m) <= (Monad m, MonadTrans t, CallM0 "ask" X2 a m) where
    callM0 base a = lift $ callM0 base a

instance CallM0 "ask" X2 a (ReaderT a m) <= Monad m where
    callM0 _ _ = ask

--instance CallM0 "ask" X2 a m <= MonadReader a m where
--    callM0 _ _ = ask

--instance CallM0 (name :: Symbol) base m a where
--    callM0 name base = lift $ callM0 name base

--xxx name base = lift $ callM0 name base

--instance CallM0 name X1 out <= (CallM0 name Identity out) where
--    callM0 name = lift $ callM0 name


--testx = do
--    lift

test = do
    x <- callM0 (Proxy :: Proxy "get") X1
    y <- callM0 (Proxy :: Proxy "ask") X2
    print' "hello"
    return x

main = do
    --print $ runState (runTestMonad test) 0
    --print $ runState test 0

    putStrLn ""
    putStrLn "-----"

    print =<< runReaderT (runStateT' test 1) 0
    print =<< runStateT' (runReaderT test 0) 1
    --print =<< runReaderT (runStateT' testM2' 0) 0


    putStrLn ""
