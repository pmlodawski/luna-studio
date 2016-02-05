{-# LANGUAGE UndecidableInstances #-}


module Luna.Syntax.Model.Graph.Builder.Class where

import Prologue hiding (Getter, Setter, read, (#))

import           Data.Prop
import           Control.Monad.Catch            (MonadMask, MonadCatch, MonadThrow)
import           Data.Construction
import           Data.Container
import           Data.Index
import           Luna.Syntax.Model.Graph
import qualified Control.Monad.State            as State
import           Data.Layer.Cover



---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Declarations === --

type    Builder  n e     = BuilderT n e Identity
newtype BuilderT n e m a = BuilderT (State.StateT (Graph n e) m a)
                              deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans
                              	       , Alternative, MonadFix, MonadMask, MonadCatch, MonadThrow)

makeWrapped ''BuilderT


-- === Utils === --

runT  ::            BuilderT n e m a -> Graph n e -> m (a, Graph n e)
evalT :: Monad m => BuilderT n e m a -> Graph n e -> m a
execT :: Monad m => BuilderT n e m a -> Graph n e -> m (Graph n e)

runT  = State.runStateT  . unwrap' ; {-# INLINE runT  #-}
evalT = State.evalStateT . unwrap' ; {-# INLINE evalT #-}
execT = State.execStateT . unwrap' ; {-# INLINE execT #-}

run  :: Builder n e a -> Graph n e -> (a, Graph n e)
eval :: Builder n e a -> Graph n e -> a
exec :: Builder n e a -> Graph n e -> Graph n e

run   = runIdentity .: runT  ; {-# INLINE run  #-}
eval  = runIdentity .: evalT ; {-# INLINE eval #-}
exec  = runIdentity .: execT ; {-# INLINE exec #-}

with :: MonadBuilder n e m => (Graph n e -> Graph n e) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadBuilder n e m => (Graph n e -> (a, Graph n e)) -> m a
modify = modifyM . fmap return
{-# INLINE modify #-}

modifyM :: MonadBuilder n e m => (Graph n e -> m (a, Graph n e)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a
{-# INLINE modifyM #-}

modify_ :: MonadBuilder n e m => (Graph n e -> Graph n e) -> m ()
modify_ = modify . fmap ((),)
{-# INLINE modify_ #-}


-- === Instances === --

class Monad m => MonadBuilder n e m | m -> n e where
    get :: m (Graph n e)
    put :: (Graph n e) -> m ()

instance Monad m => MonadBuilder n e (BuilderT n e m) where
    get = BuilderT   State.get ; {-# INLINE get #-}
    put = BuilderT . State.put ; {-# INLINE put #-}

instance State.MonadState s m => State.MonadState s (BuilderT n e m) where
    get = BuilderT $ lift   State.get ; {-# INLINE get #-}
    put = BuilderT . lift . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadBuilder n e m, MonadTrans t, Monad (t m)) => MonadBuilder n e (t m) where
    get = lift get   ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<
