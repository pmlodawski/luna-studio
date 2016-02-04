{-# LANGUAGE CPP                       #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE RankNTypes                #-}


module Luna.Syntax.Model.Network.Builder (module Luna.Syntax.Model.Network.Builder, module X) where


import Prologue hiding (Getter, Setter, read, (#))

import Luna.Syntax.Model.Graph.Builder.Class as X
import Luna.Syntax.Model.Network.Builder.Term  as X

import Data.Attribute
import Control.Monad.Catch            (MonadMask, MonadCatch, MonadThrow)
import Data.Construction
import Data.Container
import Data.Index
import Luna.Syntax.Model.Graph
import Data.Layer.Cover
import Luna.Syntax.Model.Graph.Builder.Ref

import qualified Control.Monad.State as State





-- === Utils === --


follow :: (Reader m (Edge src tgt), Functor m) => Ref (Edge src tgt) -> m (Ref $ Node tgt)
follow ptr = view target <$> read ptr

reconnect :: (Reader m n1, Writer m n1, Connectible (Ref n1) (Ref n2) m, e ~ Connection (Ref n1) (Ref n2), Unregister m e)
          => Ref n1 -> Lens' n1 e -> Ref n2 -> m e
reconnect srcRef l tgtRef = do
    src  <- read srcRef
    unregister $ src ^. l
    conn <- connection srcRef tgtRef
    write srcRef $ src & l .~ conn
    return conn


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


instance (MonadBuilder n e m, Reader m (Node node), Getter Inputs node, Unregister m inp, Attr Inputs node ~ [inp], CoverDestructor m (Node node))
      => Destructor m (Ref $ Node node) where
    destruct ref = do
        n <- read ref
        mapM_ unregister $ n # Inputs
        destructCover n
        unregister ref



