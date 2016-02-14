{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Referenced.Edge where

import Prelude.Luna

import Data.Graph.Referenced.Ref
import Data.Graph.Model.Node

import Control.Monad.Event
import Data.Container          hiding (Impossible)
import Data.Direction
import Data.Graph.Model.Edge
import Data.Index
import Data.Prop
import Luna.Evaluation.Runtime as Runtime
import Type.Bool

------------------
-- === Edge === --
------------------

-- === Definitions === --

--data Edge src tgt = Edge (Ref src) (Ref tgt) deriving (Show, Eq, Ord)
type Link       a = Arcx a a

newtype Arrow tgt     = Arrow (Ref tgt)           deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data    Arcx   src tgt = Arcx   (Ref src) (Ref tgt) deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
--newtype Link  t       = Link  (Ref t)             deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

type family Connection src tgt


-- === Utils === --

--edge ::


edge :: Ref (Node src) -> Ref (Node tgt) -> Edge $ Arcx src tgt
edge src tgt = Edge $ Arcx (rewrap src) (rewrap tgt)

source :: Lens' (Edge (Arcx src tgt)) (Ref (Node src))
source = lens (\(Edge (Arcx src _)) -> rewrap src) (\(Edge (Arcx _ tgt)) src -> Edge $ Arcx (rewrap src) tgt)

target :: Lens' (Edge (Arcx src tgt)) (Ref (Node tgt))
target = lens (\(Edge (Arcx _ tgt)) -> rewrap tgt) (\(Edge (Arcx src _)) tgt -> Edge $ Arcx src (rewrap tgt))


-- === Instances === --

-- Primitive

type instance Target (Ref (Edge (Arcx src tgt))) = Ref (Node tgt)
type instance Source (Ref (Edge (Arcx src tgt))) = Ref (Node src)

-- Connections

type instance Connection (Ref  a) (Ref  b) = Ref (Connection a b)
type instance Connection (Node a) (Node b) = Edge $ Arcx a b

-- Conversions

instance (Castable (Ref src) (Ref src'), Castable (Ref tgt) (Ref tgt')) => Castable (Arcx src tgt) (Arcx src' tgt') where
    cast (Arcx src tgt) = Arcx (cast src) (cast tgt) ; {-# INLINE cast #-}

