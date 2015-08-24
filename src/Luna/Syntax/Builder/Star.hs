{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Builder.Star where

import           Flowbox.Prelude
import qualified Control.Monad.State as State
import           Control.Monad.Fix

-- TODO: template haskellize
-- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

newtype StarBuilderT g m a = StarBuilderT { fromStarBuilderT :: State.StateT g m a }
                             deriving (Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative, MonadFix)

type StarBuilder g = StarBuilderT g Identity

class Monad m => MonadStarBuilder g m | m -> g where
    get :: m g
    put :: g -> m ()

instance Monad m => MonadStarBuilder g (StarBuilderT g m) where
    get = StarBuilderT State.get
    put = StarBuilderT . State.put

instance State.MonadState s m => State.MonadState s (StarBuilderT g m) where
    get = StarBuilderT (lift State.get)
    put = StarBuilderT . lift . State.put

instance {-# OVERLAPPABLE #-} (MonadStarBuilder g m, MonadTrans t, Monad (t m)) => MonadStarBuilder g (t m) where
    get = lift get
    put = lift . put

runT  ::            StarBuilderT g m a -> g -> m (a, g)
evalT :: Monad m => StarBuilderT g m a -> g -> m a
execT :: Monad m => StarBuilderT g m a -> g -> m g

runT  = State.runStateT  . fromStarBuilderT
evalT = State.evalStateT . fromStarBuilderT
execT = State.execStateT . fromStarBuilderT


run  :: StarBuilder g a -> g -> (a, g)
eval :: StarBuilder g a -> g -> a
exec :: StarBuilder g a -> g -> g

run   = runIdentity .: runT
eval  = runIdentity .: evalT
exec  = runIdentity .: execT

with :: MonadStarBuilder g m => (g -> g) -> m b -> m b
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out

modify :: MonadStarBuilder g m => (g -> (g, a)) -> m a
modify f = do
    s <- get
    let (s', a) = f s
    put $ s'
    return a

modify_ :: MonadStarBuilder g m => (g -> g) -> m ()
modify_ = modify . fmap (,())


-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<