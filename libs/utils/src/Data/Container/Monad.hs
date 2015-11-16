{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Container.Monad where


import qualified Control.Monad.State as State
import Prologue
import Data.STRef
import Control.Monad.Base
import GHC.ST




--instance Monad m => MonadContainer c (ContainerStateT c m) where
--    get = ContainerStateT State.get
--    put = ContainerStateT . State.put

--instance {-# OVERLAPPABLE #-} (MonadContainer g m, MonadTrans t, Monad (t m)) => MonadContainer g (t m) where
--    get = lift get
--    put = lift . put

---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

--newtype ContainerStateT c m a = ContainerStateT { fromContainerStateT :: State.StateT c m a }
--                             deriving (Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative, MonadFix)

--type ContainerState c = ContainerStateT c Identity

--class Monad m => MonadContainer c m | m -> c where
--    get :: m c
--    put :: c -> m ()



--instance State.MonadState s m => State.MonadState s (ContainerStateT g m) where
--    get = ContainerStateT $ lift   State.get
--    put = ContainerStateT . lift . State.put

----instance {-# OVERLAPPABLE #-} (MonadContainer g m, MonadTrans t, Monad (t m)) => MonadContainer g (t m) where
----    get = lift get
----    put = lift . put

----runT  ::            ContainerStateT g m a -> c -> m (a, c)
----evalT :: Monad m => ContainerStateT g m a -> c -> m a
----execT :: Monad m => ContainerStateT g m a -> c -> m c

----runT  = State.runStateT  . fromContainerStateT
----evalT = State.evalStateT . fromContainerStateT
----execT = State.execStateT . fromContainerStateT


----run  :: ContainerState g a -> c -> (a, c)
----eval :: ContainerState g a -> c -> a
----exec :: ContainerState g a -> c -> c

----run   = runIdentity .: runT
----eval  = runIdentity .: evalT
----exec  = runIdentity .: execT

----with :: MonadContainer g m => (c -> c) -> m b -> m b
----with f m = do
----    s <- get
----    put $ f s
----    out <- m
----    put s
----    return out

----modify :: MonadContainer g m => (c -> (c, a)) -> m a
----modify = modifyM . fmap return

----modifyM :: MonadContainer g m => (c -> m (c, a)) -> m a
----modifyM f = do
----    s <- get
----    (s', a) <- f s
----    put $ s'
----    return a

----modify_ :: MonadContainer g m => (c -> c) -> m ()
----modify_ = modify . fmap (,())

---- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<


---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

--newtype ContainerRefT s c m a = ContainerRefT { fromContainerRefT :: State.StateT (STRef s c) m a }
--                             deriving (Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative, MonadFix)

--type ContainerRef s c = ContainerRefT s c Identity

--instance MonadBase (ST s) m => MonadContainer c (ContainerRefT s c m) where
--    get   = ContainerRefT $ liftBase . readSTRef =<< State.get
--    put a = ContainerRefT $ do
--        ref <- State.get
--        liftBase $ writeSTRef ref a


--instance State.MonadState s m => State.MonadState s (ContainerRefT s c m) where
--    get = ContainerRefT $ lift   State.get
--    put = ContainerRefT . lift . State.put



--runT  ::            ContainerRefT s c m a -> STRef s c -> m (a, STRef s c)
--evalT :: Monad m => ContainerRefT s c m a -> STRef s c -> m a
--execT :: Monad m => ContainerRefT s c m a -> STRef s c -> m (STRef s c)

----runT :: _ => _
--runT  = State.runStateT  . fromContainerRefT
--evalT = State.evalStateT . fromContainerRefT
--execT = State.execStateT . fromContainerRefT


----run  :: ContainerRef g a -> BldrState g -> (a, BldrState g)
----eval :: ContainerRef g a -> BldrState g -> a
----exec :: ContainerRef g a -> BldrState g -> (BldrState g)

----run   = runIdentity .: runT
----eval  = runIdentity .: evalT
----exec  = runIdentity .: execT

----with :: MonadContainerRef g m => (BldrState g -> BldrState g) -> m b -> m b
----with f m = do
----    s <- get
----    put $ f s
----    out <- m
----    put s
----    return out

----modify :: MonadContainerRef g m => (BldrState g -> (BldrState g, a)) -> m a
----modify = modifyM . fmap return

----modifyM :: MonadContainerRef g m => (BldrState g -> m (BldrState g, a)) -> m a
----modifyM f = do
----    s <- get
----    (s', a) <- f s
----    put $ s'
----    return a

----modify_ :: MonadContainerRef g m => (BldrState g -> BldrState g) -> m ()
----modify_ = modify . fmap (,())

---- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<