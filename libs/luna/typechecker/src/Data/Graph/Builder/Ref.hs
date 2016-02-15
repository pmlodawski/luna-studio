{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Builder.Ref where

import Prelude.Luna

import Data.Graph.Builders
import Data.Graph
import Luna.Syntax.Model.Graph
import Data.Construction
import Data.Prop
import Data.Index
import Data.Container
import Data.Graph.Builder.Class
import Data.Graph.Backend.Vector
import Data.Graph.Referenced


-- === Utils === --

type RefHandler m a = (Reader m a, Writer m a)
class Monad m => Reader m a where read  :: Ref a -> m a
class Monad m => Writer m a where write :: Ref a -> a -> m ()

withM :: RefHandler m a => Ref a -> (a -> m a) -> m ()
withM ref f = read ref >>= f >>= write ref

with :: RefHandler m a => Ref a -> (a -> a) -> m ()
with ref = withM ref ∘ (return <$>)

follow :: Reader m (Edge (Arc src tgt)) => Lens' (Edge (Arc src tgt)) t -> Ref (Edge (Arc src tgt)) -> m t
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

instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable a n) => Constructor m (Ref $ Node a) where
    construct n = Ref <$> modify (wrapped' ∘ nodeGraph $ swap ∘ ixed add (cast $ unwrap' n)) ; {-# INLINE construct #-}

instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable (Edge e') e) => Constructor m (Ref $ Edge e') where
    construct e = Ref <$> modify (wrapped' ∘ edgeGraph $ swap ∘ ixed add (cast e)) ; {-# INLINE construct #-}

instance MonadBuilder (Hetero (VectorGraph n e)) m => Constructor m (Ref Cluster) where
    construct c = Ref <$> modify (wrapped' ∘ clusterGraph $ swap ∘ ixed add c) ; {-# INLINE construct #-}

-- Accessors

instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable n a)              => Reader m (Node a)       where read  = flip fmap get ∘ getter ; {-# INLINE read #-}
instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable e e')             => Reader m (Edge e')      where read  = flip fmap get ∘ getter ; {-# INLINE read #-}
instance  MonadBuilder (Hetero (VectorGraph n e)) m                             => Reader m Cluster        where read  = flip fmap get ∘ getter ; {-# INLINE read #-}

instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable a n)              => Writer m (Node a)       where write = modify_ ∘∘ setter ; {-# INLINE write #-}
instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable e' e)             => Writer m (Edge e')      where write = modify_ ∘∘ setter ; {-# INLINE write #-}
instance  MonadBuilder (Hetero (VectorGraph n e)) m                             => Writer m Cluster        where write = modify_ ∘∘ setter ; {-# INLINE write #-}

-- Unregistering

instance MonadBuilder (Hetero (VectorGraph n e)) m => Unregister m (Ref $ Node node) where unregister ref = modify_ $ wrapped' ∘ nodeGraph %~ free (ref ^. idx)
instance MonadBuilder (Hetero (VectorGraph n e)) m => Unregister m (Ref $ Edge edge) where unregister ref = modify_ $ wrapped' ∘ edgeGraph %~ free (ref ^. idx)

-- Destruction

instance (MonadBuilder (Hetero (VectorGraph n e)) m, Reader m (Node node), Getter Inputs node, Unregister m inp, Prop Inputs node ~ [inp], Destructor m node)
      => Destructor m (Ref $ Node node) where
    destruct ref = do
        n <- read ref
        mapM_ unregister $ n # Inputs
        destruct n
        unregister ref
    {-# INLINE destruct #-}

instance Destructor m n => Destructor m (Node n) where destruct = destruct ∘ unwrap' ; {-# INLINE destruct #-}
