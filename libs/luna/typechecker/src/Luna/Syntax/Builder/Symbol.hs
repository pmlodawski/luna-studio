{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Luna.Syntax.Builder.Symbol where

import Prologue

import qualified Control.Monad.Catch      as Catch
import qualified Control.Monad.State      as State
import qualified Language.Haskell.Session as HS


-- TODO: template haskellize
-- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

newtype SymbolT g m a = SymbolT { fromSymbolT :: State.StateT g m a }
                        deriving (Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative, MonadFix, HS.GhcMonad, HS.ExceptionMonad, HS.HasDynFlags, Catch.MonadMask, Catch.MonadCatch, Catch.MonadThrow)

type Builder g = SymbolT g Identity

class Monad m => MonadSymbolBuilder g m | m -> g where
    get :: m g
    put :: g -> m ()

instance Monad m => MonadSymbolBuilder g (SymbolT g m) where
    get = SymbolT State.get
    put = SymbolT . State.put

instance State.MonadState s m => State.MonadState s (SymbolT g m) where
    get = SymbolT (lift State.get)
    put = SymbolT . lift . State.put

instance {-# OVERLAPPABLE #-} (MonadSymbolBuilder g m, MonadTrans t, Monad (t m)) => MonadSymbolBuilder g (t m) where
    get = lift get
    put = lift . put

runT  ::            SymbolT g m a -> g -> m (a, g)
evalT :: Monad m => SymbolT g m a -> g -> m a
execT :: Monad m => SymbolT g m a -> g -> m g

runT  = State.runStateT  . fromSymbolT
evalT = State.evalStateT . fromSymbolT
execT = State.execStateT . fromSymbolT


run  :: Builder g a -> g -> (a, g)
eval :: Builder g a -> g -> a
exec :: Builder g a -> g -> g

run   = runIdentity .: runT
eval  = runIdentity .: evalT
exec  = runIdentity .: execT

with :: MonadSymbolBuilder g m => (g -> g) -> m b -> m b
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out

modify :: MonadSymbolBuilder g m => (g -> (g, a)) -> m a
modify = modifyM . fmap return

modify2 :: MonadSymbolBuilder g m => (g -> (a, g)) -> m a
modify2 = modifyM2 . fmap return

modifyM :: MonadSymbolBuilder g m => (g -> m (g, a)) -> m a
modifyM f = do
    s <- get
    (s', a) <- f s
    put $ s'
    return a

modifyM2 :: MonadSymbolBuilder g m => (g -> m (a, g)) -> m a
modifyM2 f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a

modify_ :: MonadSymbolBuilder g m => (g -> g) -> m ()
modify_ = modify . fmap (,())

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

withSymbols :: MonadSymbolBuilder g m => (g -> (g, a)) -> m a
withSymbols = withSymbolsM . fmap return

withSymbols' :: MonadSymbolBuilder g m => (g -> (a, g)) -> m a
withSymbols' = withSymbolsM . fmap (return . switch')

withSymbols_ :: MonadSymbolBuilder g m => (g -> g) -> m ()
withSymbols_ = withSymbols . fmap (,())

withSymbolsM :: MonadSymbolBuilder g m => (g -> m (g, a)) -> m a
withSymbolsM = modifyM

withSymbolsM_ :: MonadSymbolBuilder g m => (g -> m g) -> m ()
withSymbolsM_ = withSymbolsM . (fmap . fmap) (,())

runSymbolT  :: Functor m => SymbolT g m a -> g -> m (a, g)
execSymbolT :: Monad   m => SymbolT g m a -> g -> m g
evalSymbolT :: Monad   m => SymbolT g m a -> g -> m a

runSymbolT  = runT
execSymbolT = execT
evalSymbolT = evalT




switch' (a,b) = (b,a)
