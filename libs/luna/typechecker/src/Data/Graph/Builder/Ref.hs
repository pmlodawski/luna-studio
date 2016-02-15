{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Builder.Ref where

import Prelude.Luna

import Data.Graph.Builders
import Data.Graph
import Data.Construction
import Data.Prop
import Data.Index
import Data.Container
import Data.Graph.Builder.Class
import Data.Graph.Backend.Vector
import Data.Graph.Referenced


-- === Utils === --

--type RefHandler r m a = (Reader r m a, Writer r m a)
--class Monad m => Reader r m a where read  :: Ref r a -> m a
--class Monad m => Writer r m a where write :: Ref r a -> a -> m ()


withM :: (MonadBuilder t m, HasRef r a t) => Ref r a -> (a -> m a) -> m ()
withM ref f = read ref >>= f >>= write ref

with :: (MonadBuilder t m, HasRef r a t) => Ref r a -> (a -> a) -> m ()
with ref = withM ref ∘ (return <$>)

follow :: (MonadBuilder t m, HasRef r a t) => Lens' a b -> Ref r a -> m b
follow f ptr = view f <$> read ptr

reconnect :: --(RefHandler m el, Connectible (Ref inp) (Ref el) m, conn ~ Connection (Ref inp) (Ref el), Unregister m conn)
          (MonadBuilder t m, HasRef r el t, Unregister m conn, Connectible' (Ref r inp) (Ref r el) m conn) => Ref r el -> Lens' el conn -> Ref r inp -> m conn
reconnect elRef lens input = do
    el  <- read elRef
    unregister $ el ^. lens
    conn <- connection input elRef
    write elRef $ el & lens .~ conn
    return conn


--class HasRef r a t where ref :: Ref r a -> Lens' t a

read :: (MonadBuilder t m, HasRef r a t) => Ref r a -> m a
read r = view (ref r) <$> get

write :: (MonadBuilder t m, HasRef r a t) => Ref r a -> a -> m ()
write r = modify_ ∘ set (ref r)

-- === Instances === --

-- Construction

instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable a n) => Constructor m (Ref Node a) where
    construct n = Ref <$> modify (wrapped' ∘ nodeGraph $ swap ∘ ixed add (cast n)) ; {-# INLINE construct #-}

instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable a e) => Constructor m (Ref Edge a) where
    construct e = Ref <$> modify (wrapped' ∘ edgeGraph $ swap ∘ ixed add (cast e)) ; {-# INLINE construct #-}

    --instance MonadBuilder (Hetero (VectorGraph n e)) m => Constructor m (Ref Cluster) where
    --    construct c = Ref <$> modify (wrapped' ∘ clusterGraph $ swap ∘ ixed add c) ; {-# INLINE construct #-}

-- Accessors

--instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable n a)              => Reader r m (Node a)       where read  = flip fmap get ∘ getter ; {-# INLINE read #-}
--instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable e e')             => Reader r m (Edge e')      where read  = flip fmap get ∘ getter ; {-# INLINE read #-}
--instance  MonadBuilder (Hetero (VectorGraph n e)) m                             => Reader r m Cluster        where read  = flip fmap get ∘ getter ; # INLINE read #

--instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable a n)              => Writer r m (Node a)       where write = modify_ ∘∘ setter ; {-# INLINE write #-}
--instance (MonadBuilder (Hetero (VectorGraph n e)) m, Castable e' e)             => Writer r m (Edge e')      where write = modify_ ∘∘ setter ; {-# INLINE write #-}
--instance  MonadBuilder (Hetero (VectorGraph n e)) m                             => Writer r m Cluster        where write = modify_ ∘∘ setter ; {-# INLINE write #-}

-- Unregistering

-- FXIME[WD]: hardcoded graph type
instance MonadBuilder (Hetero (VectorGraph n e)) m => Unregister m (Ref Node node) where unregister ref = modify_ $ wrapped' ∘ nodeGraph %~ free (ref ^. idx)
instance MonadBuilder (Hetero (VectorGraph n e)) m => Unregister m (Ref Edge edge) where unregister ref = modify_ $ wrapped' ∘ edgeGraph %~ free (ref ^. idx)

-- Destruction

instance (MonadBuilder t m, Prop Inputs node ~ [inp], HasRef Node node t, Unregister m inp, Getter Inputs node, Destructor m node, Unregister m (Ref Node node))
      => Destructor m (Ref Node node) where
    destruct ref = do
        n <- read ref
        mapM_ unregister $ n # Inputs
        destruct n
        unregister ref
    {-# INLINE destruct #-}

--instance Destructor m n => Destructor m (Node n) where destruct = destruct ∘ unwrap' ; {-# INLINE destruct #-}
