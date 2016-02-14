module Data.Graph.Model.Hetero (module Data.Graph.Model.Hetero, module X) where

import Prologue hiding (Getter, Setter)

import Data.Container.Hetero as X (Hetero(..))
import Data.Prop

-- === Instances === --

-- Properties

type instance Prop p (Hetero a) = Prop p a
instance Getter p a => Getter p (Hetero a) where getter p   = getter p ∘ unwrap'
instance Setter p a => Setter p (Hetero a) where setter p v = wrapped' %~ setter p v
