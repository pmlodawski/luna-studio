{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Graph.Builder.Ref where

import Prelude.Luna

import Data.Graph.Builders
import Data.Graph
import Luna.Syntax.Model.Graph
import Data.Construction
import Data.Prop
import Data.Index
import Luna.Syntax.Model.Graph.Builder.Class
import Data.Container
import Data.Graph.Backend.Vector


-- === Utils === --

type RefHandler m a = (Reader m a, Writer m a)
class Monad m => Reader m a where read  :: Ref a -> m a
class Monad m => Writer m a where write :: Ref a -> a -> m ()

withM :: RefHandler m a => Ref a -> (a -> m a) -> m ()
withM ref f = read ref >>= f >>= write ref

with :: RefHandler m a => Ref a -> (a -> a) -> m ()
with ref = withM ref ∘ (return <$>)

follow :: Reader m (Arc src tgt) => Lens' (Arc src tgt) t -> Ref (Arc src tgt) -> m t
follow f ptr = view f <$> read ptr

reconnect :: (RefHandler m el, Connectible (Ref inp) (Ref el) m, conn ~ Connection (Ref inp) (Ref el), Unregister m conn)
          => Ref el -> Lens' el conn -> Ref inp -> m conn
reconnect elRef lens input = do
    el  <- read elRef
    unregister $ el ^. lens
    conn <- connection input elRef
    write elRef $ el & lens .~ conn
    return conn


-- === Instances === --

-- Construction

instance (MonadBuilder n e m, Castable a n) => Constructor m (Ref $ Node a) where
    construct n = Ref <$> modify (nodeGraph $ swap ∘ ixed add (cast $ unwrap' n)) ; {-# INLINE construct #-}

instance (MonadBuilder n e m, Castable (Arc src tgt) e) => Constructor m (Ref $ Arc src tgt) where
    construct e = Ref <$> modify (edgeGraph $ swap ∘ ixed add (cast e)) ; {-# INLINE construct #-}

instance MonadBuilder n e m => Constructor m (Ref Cluster) where
    construct c = Ref <$> modify (clusterGraph $ swap ∘ ixed add c) ; {-# INLINE construct #-}

-- Accessors

instance (MonadBuilder n e m, Castable n a)              => Reader m (Node a)       where read  = flip fmap get ∘ getter ; {-# INLINE read #-}
instance (MonadBuilder n e m, Castable e (Arc src tgt)) => Reader m (Arc src tgt) where read  = flip fmap get ∘ getter ; {-# INLINE read #-}
instance  MonadBuilder n e m                             => Reader m Cluster        where read  = flip fmap get ∘ getter ; {-# INLINE read #-}

instance (MonadBuilder n e m, Castable a n)              => Writer m (Node a)       where write = modify_ ∘∘ setter ; {-# INLINE write #-}
instance (MonadBuilder n e m, Castable (Arc src tgt) e) => Writer m (Arc src tgt) where write = modify_ ∘∘ setter ; {-# INLINE write #-}
instance  MonadBuilder n e m                             => Writer m Cluster        where write = modify_ ∘∘ setter ; {-# INLINE write #-}

-- Unregistering

instance MonadBuilder n e m => Unregister m (Ref $ Node node)    where unregister ref = modify_ $ nodeGraph %~ free (ref ^. idx)
instance MonadBuilder n e m => Unregister m (Ref $ Arc src dst) where unregister ref = modify_ $ edgeGraph %~ free (ref ^. idx)

-- Destruction

instance (MonadBuilder n e m, Reader m (Node node), Getter Inputs node, Unregister m inp, Prop Inputs node ~ [inp], Destructor m node)
      => Destructor m (Ref $ Node node) where
    destruct ref = do
        n <- read ref
        mapM_ unregister $ n # Inputs
        destruct n
        unregister ref
    {-# INLINE destruct #-}

instance Destructor m n => Destructor m (Node n) where destruct = destruct ∘ unwrap' ; {-# INLINE destruct #-}
