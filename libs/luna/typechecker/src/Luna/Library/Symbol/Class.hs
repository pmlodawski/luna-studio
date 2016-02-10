{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Luna.Library.Symbol.Class where

import Prologue  hiding (Symbol)

import qualified Control.Monad.State           as State
import           Control.Monad.Catch           (MonadMask, MonadCatch, MonadThrow)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Luna.Syntax.AST.Decl.Function (Function)
import           Luna.Library.Symbol.QualPath  (QualPath)

-- === Definitions === --

type SymbolMap n = Map QualPath (Function n)


---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Declarations === --

type    Symbol  n     = SymbolT n Identity
newtype SymbolT n m a = SymbolT (State.StateT (SymbolMap n) m a)
                              deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans
                                       , Alternative, MonadFix, MonadMask, MonadCatch, MonadThrow)

makeWrapped ''SymbolT


-- === Utils === --

runT  ::            SymbolT n m a -> SymbolMap n -> m (a, SymbolMap n)
evalT :: Monad m => SymbolT n m a -> SymbolMap n -> m a
execT :: Monad m => SymbolT n m a -> SymbolMap n -> m (SymbolMap n)

runT  = State.runStateT  . unwrap' ; {-# INLINE runT  #-}
evalT = State.evalStateT . unwrap' ; {-# INLINE evalT #-}
execT = State.execStateT . unwrap' ; {-# INLINE execT #-}

run  :: Symbol n a -> SymbolMap n -> (a, SymbolMap n)
eval :: Symbol n a -> SymbolMap n -> a
exec :: Symbol n a -> SymbolMap n -> SymbolMap n

run   = runIdentity .: runT  ; {-# INLINE run  #-}
eval  = runIdentity .: evalT ; {-# INLINE eval #-}
exec  = runIdentity .: execT ; {-# INLINE exec #-}

with :: MonadSymbol n m => (SymbolMap n -> SymbolMap n) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadSymbol n m => (SymbolMap n -> (a, SymbolMap n)) -> m a
modify = modifyM . fmap return
{-# INLINE modify #-}

modifyM :: MonadSymbol n m => (SymbolMap n -> m (a, SymbolMap n)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a
{-# INLINE modifyM #-}

modify_ :: MonadSymbol n m => (SymbolMap n -> SymbolMap n) -> m ()
modify_ = modify . fmap ((),)
{-# INLINE modify_ #-}


-- === Instances === --

class Monad m => MonadSymbol n m | m -> n where
    get :: m (SymbolMap n)
    put :: SymbolMap n -> m ()

instance Monad m => MonadSymbol n (SymbolT n m) where
    get = SymbolT   State.get ; {-# INLINE get #-}
    put = SymbolT . State.put ; {-# INLINE put #-}

instance State.MonadState s m => State.MonadState s (SymbolT n m) where
    get = SymbolT $ lift   State.get ; {-# INLINE get #-}
    put = SymbolT . lift . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadSymbol n m, MonadTrans t, Monad (t m)) => MonadSymbol n (t m) where
    get = lift get   ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

-- === Behaviors === --

loadSymbols :: MonadSymbol n m => SymbolMap n -> m ()
loadSymbols = modify_ . Map.union

lookupSymbol :: MonadSymbol n m => QualPath -> m (Maybe (Function n))
lookupSymbol p = Map.lookup p <$> get
