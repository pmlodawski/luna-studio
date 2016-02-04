{-# LANGUAGE CPP                       #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE RankNTypes                #-}


module Luna.Syntax.Model.Graph.Builder where


import Prologue hiding (Getter, Setter, read, (#))

import           Data.Attribute
import           Control.Monad.Catch            (MonadMask, MonadCatch, MonadThrow)
import           Data.Construction
import           Data.Container
import           Data.Index
import           Luna.Syntax.Model.Graph.Class
import qualified Control.Monad.State            as State
import           Luna.Syntax.Model.Graph.Layers




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


-- === Instances === --

-- Ref construction

instance (MonadBuilder n e m, Castable a n) => Constructor m (Ref $ Node a) where 
    construct n = Ref ∘ Ptr <$> modify (nodes $ swap ∘ ixed add (cast ast)) where
        ast = unwrap' n :: a

instance (MonadBuilder n e m, Castable (Edge src tgt) e) => Constructor m (Ref $ Edge src tgt) where 
    construct e = Ref ∘ Ptr <$> modify (edges $ swap ∘ ixed add (cast e)) where

-- Ref reading / writing

instance (MonadBuilder n e m, Castable n a) => Reader m (Node a) where
    read ref = Node ∘ cast ∘ index_ (ref ^. idx) ∘ view nodes <$> get ; {-# INLINE read #-}

instance (MonadBuilder n e m, Castable a n) => Writer m (Node a) where
    write ref val = modify_ $ nodes %~ unchecked inplace insert_ (ref ^. idx) (cast $ unwrap' val) ; {-# INLINE write #-}

instance (MonadBuilder n e m, Castable e (Edge src tgt)) => Reader m (Edge src tgt) where
    read ref = cast ∘ index_ (ref ^. idx) ∘ view edges <$> get ; {-# INLINE read #-}





instance MonadBuilder n e m => Unregister m (Ref $ Node node)    where unregister ref = modify_ $ nodes %~ free (ref ^. idx)
instance MonadBuilder n e m => Unregister m (Ref $ Edge src dst) where unregister ref = modify_ $ edges %~ free (ref ^. idx)


instance (MonadBuilder n e m, Reader m (Node node), Getter Inputs node, Unregister m inp, Attr Inputs node ~ [inp] )
      => Destroyer m (Ref $ Node node) where
    destroy ref = do
        n <- read ref
        mapM_ unregister $ n # Inputs
        unregister ref

reconnect :: (Reader m n1, Writer m n1, Connectible (Ref n1) (Ref n2) m, e ~ Connection (Ref n1) (Ref n2), Unregister m e)
          => Ref n1 -> Lens' n1 e -> Ref n2 -> m e
reconnect srcRef l tgtRef = do
    src  <- read srcRef
    unregister $ src ^. l
    conn <- connection srcRef tgtRef
    write srcRef $ src & l .~ conn
    return conn
