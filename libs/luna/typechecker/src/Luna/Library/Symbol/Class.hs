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

type SymbolMap n g = Map QualPath (Function n)


---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Declarations === --

type    Symbol  n g     = SymbolT n g Identity
newtype SymbolT n g m a = SymbolT (State.StateT (SymbolMap n g) m a)
                              deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans
                                       , Alternative, MonadFix, MonadMask, MonadCatch, MonadThrow)

makeWrapped ''SymbolT


-- === Utils === --

runT  ::            SymbolT n g m a -> SymbolMap n g -> m (a, SymbolMap n g)
evalT :: Monad m => SymbolT n g m a -> SymbolMap n g -> m a
execT :: Monad m => SymbolT n g m a -> SymbolMap n g -> m (SymbolMap n g)

runT  = State.runStateT  . unwrap' ; {-# INLINE runT  #-}
evalT = State.evalStateT . unwrap' ; {-# INLINE evalT #-}
execT = State.execStateT . unwrap' ; {-# INLINE execT #-}

run  :: Symbol n g a -> SymbolMap n g -> (a, SymbolMap n g)
eval :: Symbol n g a -> SymbolMap n g -> a
exec :: Symbol n g a -> SymbolMap n g -> SymbolMap n g

run   = runIdentity .: runT  ; {-# INLINE run  #-}
eval  = runIdentity .: evalT ; {-# INLINE eval #-}
exec  = runIdentity .: execT ; {-# INLINE exec #-}

with :: MonadSymbol n g m => (SymbolMap n g -> SymbolMap n g) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadSymbol n g m => (SymbolMap n g -> (a, SymbolMap n g)) -> m a
modify = modifyM . fmap return
{-# INLINE modify #-}

modifyM :: MonadSymbol n g m => (SymbolMap n g -> m (a, SymbolMap n g)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a
{-# INLINE modifyM #-}

modify_ :: MonadSymbol n g m => (SymbolMap n g -> SymbolMap n g) -> m ()
modify_ = modify . fmap ((),)
{-# INLINE modify_ #-}


-- === Instances === --

class Monad m => MonadSymbol n g m | m -> n, m -> g where
    get :: m (SymbolMap n g)
    put :: SymbolMap n g -> m ()

instance Monad m => MonadSymbol n g (SymbolT n g m) where
    get = SymbolT   State.get ; {-# INLINE get #-}
    put = SymbolT . State.put ; {-# INLINE put #-}

instance State.MonadState s m => State.MonadState s (SymbolT n g m) where
    get = SymbolT $ lift   State.get ; {-# INLINE get #-}
    put = SymbolT . lift . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadSymbol n g m, MonadTrans t, Monad (t m)) => MonadSymbol n g (t m) where
    get = lift get   ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

-- === Behaviors === --

loadSymbols :: MonadSymbol n g m => SymbolMap n g -> m ()
loadSymbols = modify_ . Map.union

lookupSymbol :: MonadSymbol n g m => QualPath -> m (Maybe (Function n))
lookupSymbol p = Map.lookup p <$> get
