{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Luna.Syntax.Builder.Class where

import Prologue

import qualified Control.Monad.Catch      as Catch
import qualified Control.Monad.State      as State
import qualified Language.Haskell.Session as HS


-- TODO: template haskellize
-- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

newtype BuilderT g m a = BuilderT { fromBuilderT :: State.StateT g m a }
                             deriving (Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative, MonadFix, HS.GhcMonad, HS.ExceptionMonad, HS.HasDynFlags, Catch.MonadMask, Catch.MonadCatch, Catch.MonadThrow)

type Builder g = BuilderT g Identity

class Monad m => BuilderMonad g m | m -> g where
    get :: m g
    put :: g -> m ()

instance Monad m => BuilderMonad g (BuilderT g m) where
    get = BuilderT State.get
    put = BuilderT . State.put

instance State.MonadState s m => State.MonadState s (BuilderT g m) where
    get = BuilderT (lift State.get)
    put = BuilderT . lift . State.put

instance {-# OVERLAPPABLE #-} (BuilderMonad g m, MonadTrans t, Monad (t m)) => BuilderMonad g (t m) where
    get = lift get
    put = lift . put

runT  ::            BuilderT g m a -> g -> m (a, g)
evalT :: Monad m => BuilderT g m a -> g -> m a
execT :: Monad m => BuilderT g m a -> g -> m g

runT  = State.runStateT  . fromBuilderT
evalT = State.evalStateT . fromBuilderT
execT = State.execStateT . fromBuilderT


run  :: Builder g a -> g -> (a, g)
eval :: Builder g a -> g -> a
exec :: Builder g a -> g -> g

run   = runIdentity .: runT
eval  = runIdentity .: evalT
exec  = runIdentity .: execT

with :: BuilderMonad g m => (g -> g) -> m b -> m b
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out

modify :: BuilderMonad g m => (g -> (g, a)) -> m a
modify = modifyM . fmap return

modify2 :: BuilderMonad g m => (g -> (a, g)) -> m a
modify2 = modifyM2 . fmap return

modifyM :: BuilderMonad g m => (g -> m (g, a)) -> m a
modifyM f = do
    s <- get
    (s', a) <- f s
    put $ s'
    return a

modifyM2 :: BuilderMonad g m => (g -> m (a, g)) -> m a
modifyM2 f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a

modify_ :: BuilderMonad g m => (g -> g) -> m ()
modify_ = modify . fmap (,())

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

withGraph :: BuilderMonad g m => (g -> (g, a)) -> m a
withGraph = withGraphM . fmap return

withGraph' :: BuilderMonad g m => (g -> (a, g)) -> m a
withGraph' = withGraphM . fmap (return . switch')

withGraph_ :: BuilderMonad g m => (g -> g) -> m ()
withGraph_ = withGraph . fmap (,())

withGraphM :: BuilderMonad g m => (g -> m (g, a)) -> m a
withGraphM = modifyM

withGraphM_ :: BuilderMonad g m => (g -> m g) -> m ()
withGraphM_ = withGraphM . (fmap . fmap) (,())

runBuilderT  :: Functor m => BuilderT g m a -> g -> m (a, g)
execBuilderT :: Monad   m => BuilderT g m a -> g -> m g
evalBuilderT :: Monad   m => BuilderT g m a -> g -> m a

runBuilderT  = runT
execBuilderT = execT
evalBuilderT = evalT




switch' (a,b) = (b,a)
