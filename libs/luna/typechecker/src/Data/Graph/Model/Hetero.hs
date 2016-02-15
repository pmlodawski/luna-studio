{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Model.Hetero (module Data.Graph.Model.Hetero, module X) where

import Prologue hiding (Getter, Setter)

import Data.Container.Hetero as X (Hetero(..))
import Data.Prop
import Data.Graph.Model.Ref
import Data.Graph.Model.Edge
import Data.Graph.Model.Node

-- === Instances === --

-- Properties

type instance Prop p (Hetero a) = Prop p a
instance Getter p a => Getter p (Hetero a) where getter p   = getter p ∘ unwrap'
instance Setter p a => Setter p (Hetero a) where setter p v = wrapped' %~ setter p v

-- Hetero reference handling

-- | When referencing the Hetero graph, we query the underlying one for its native node and edge representations
--   by using `NodeOf` and `EdgeOf` families respectively.

instance (Referred Node n' a, BiCastable n n', n' ~ (a # Node))
      =>  Referred Node n (Hetero a) where focus r = wrapped' ∘ focus (cast r :: Ref Node n') ∘ casted ; {-# INLINE focus #-}
instance  Referred Node I (Hetero a) where focus   = impossible
instance  Referred Node n (Hetero I) where focus   = impossible

instance (Referred Edge e' a, BiCastable e e', e' ~ (a # Edge))
      =>  Referred Edge e (Hetero a) where focus r = wrapped' ∘ focus (cast r :: Ref Edge e') ∘ casted ; {-# INLINE focus #-}
instance  Referred Edge I (Hetero a) where focus   = impossible
instance  Referred Edge e (Hetero I) where focus   = impossible
