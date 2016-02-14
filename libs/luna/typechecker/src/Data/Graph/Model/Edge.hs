{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Model.Edge where

import Prelude.Luna

import Data.Graph.Model.Ref
import Data.Graph.Model.Node

import Data.Prop
import Control.Monad.Event
import Data.Direction
import Data.Index
import Data.Container          hiding (Impossible)
import Luna.Evaluation.Runtime as Runtime
import Type.Bool


------------------
-- === Edge === --
------------------

-- === Definitions === --

data Edge src tgt = Edge (Ref src) (Ref tgt) deriving (Show, Eq, Ord)
type Link       a = Edge a a

type family Connection src tgt


-- === Utils === --

edge :: Ref (Node src) -> Ref (Node tgt) -> Edge src tgt
edge src tgt = Edge (rewrap src) (rewrap tgt)

source :: Lens' (Edge src tgt) (Ref (Node src))
source = lens (\(Edge src _) -> rewrap src) (\(Edge _ tgt) src -> Edge (rewrap src) tgt)

target :: Lens' (Edge src tgt) (Ref (Node tgt))
target = lens (\(Edge _ tgt) -> rewrap tgt) (\(Edge src _) tgt -> Edge src (rewrap tgt))


-- === Instances === --

-- Primitive

type instance Target (Ref (Edge src tgt)) = Ref (Node tgt)
type instance Source (Ref (Edge src tgt)) = Ref (Node src)

-- Connections

type instance Connection (Ref  a) (Ref  b) = Ref (Connection a b)
type instance Connection (Node a) (Node b) = Edge a b

-- Conversions

instance (Castable (Ref src) (Ref src'), Castable (Ref tgt) (Ref tgt')) => Castable (Edge src tgt) (Edge src' tgt') where
    cast (Edge src tgt) = Edge (cast src) (cast tgt) ; {-# INLINE cast #-}
