{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Model.Edge where

import Prologue


-- === Definitions === --

data Edge = Edge deriving (Show, Eq, Ord)
--type family EdgeOf a

-- === Instances === --

-- Wrappers

--makeWrapped ''Edge
--type instance Uncovered (Edge a) = Uncovered (Unlayered (Edge a))
--type instance Unlayered (Edge a) = Unwrapped (Edge a)
--instance      Layered   (Edge a)

-- Conversions

--instance Castable a a' => Castable (Edge a) (Edge a') where
--    cast = wrapped %~ cast ; {-# INLINE cast #-}
