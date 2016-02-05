{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Graph.Builder.Ref where

import Prelude.Luna

import Luna.Syntax.Model.Graph
import Data.Construction
import Data.Prop
import Data.Index
import Luna.Syntax.Model.Graph.Builder.Class
import Data.Container



-- === Utils === --

type RefHandler m a = (Reader m a, Writer m a)
class Monad m => Reader m a where read  :: Ref a -> m a
class Monad m => Writer m a where write :: Ref a -> a -> m ()

withM :: RefHandler m a => Ref a -> (a -> m a) -> m ()
withM ref f = read ref >>= f >>= write ref

with :: RefHandler m a => Ref a -> (a -> a) -> m ()
with ref = withM ref ∘ (return <$>)


-- === Instances === --

-- Construction

instance (MonadBuilder n e m, Castable a n) => Constructor m (Ref $ Node a) where 
    construct n = Ref ∘ Ptr <$> modify (nodeGraph $ swap ∘ ixed add (cast $ unwrap' n)) ; {-# INLINE construct #-}

instance (MonadBuilder n e m, Castable (Edge src tgt) e) => Constructor m (Ref $ Edge src tgt) where 
    construct e = Ref ∘ Ptr <$> modify (edgeGraph $ swap ∘ ixed add (cast e)) ; {-# INLINE construct #-}

-- Accessors

instance (MonadBuilder n e m, Castable n a)              => Reader m (Node a)       where read  = flip fmap get ∘  getter ; {-# INLINE read  #-}
instance (MonadBuilder n e m, Castable e (Edge src tgt)) => Reader m (Edge src tgt) where read  = flip fmap get ∘  getter ; {-# INLINE read  #-}
instance (MonadBuilder n e m, Castable a n)              => Writer m (Node a)       where write = modify_       ∘∘ setter ; {-# INLINE write #-}
instance (MonadBuilder n e m, Castable (Edge src tgt) e) => Writer m (Edge src tgt) where write = modify_       ∘∘ setter ; {-# INLINE write #-}

-- Unregistering

instance MonadBuilder n e m => Unregister m (Ref $ Node node)    where unregister ref = modify_ $ nodeGraph %~ free (ref ^. idx)
instance MonadBuilder n e m => Unregister m (Ref $ Edge src dst) where unregister ref = modify_ $ edgeGraph %~ free (ref ^. idx)

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