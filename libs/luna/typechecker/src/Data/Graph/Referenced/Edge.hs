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

newtype Arrow tgt     = Arrow (Ref tgt)           deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data    Arc   src tgt = Arc   (Ref src) (Ref tgt) deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
type    Link  a       = Arc   a a

type family Connection src tgt


-- === Utils === --

--class HasSource a where

arc :: Ref (Node src) -> Ref (Node tgt) -> Arc src tgt
arc src tgt = Arc (rewrap src) (rewrap tgt)

link :: Ref (Node t) -> Ref (Node t) -> Link t
link = arc

arrow :: Ref (Node tgt) -> Arrow tgt
arrow tgt = Arrow (rewrap tgt)

edge :: Ref (Node src) -> Ref (Node tgt) -> Edge $ Arc src tgt
edge src tgt = Edge $ Arc (rewrap src) (rewrap tgt)


-- FIXME[WD]: refactor source / target to use the Data.Direction abstraction

source :: Lens' (Edge (Arc src tgt)) (Ref (Node src))
source = lens (\(Edge (Arc src _)) -> rewrap src) (\(Edge (Arc _ tgt)) src -> Edge $ Arc (rewrap src) tgt)

target :: Lens' (Edge (Arc src tgt)) (Ref (Node tgt))
target = lens (\(Edge (Arc _ tgt)) -> rewrap tgt) (\(Edge (Arc src _)) tgt -> Edge $ Arc src (rewrap tgt))


-- === Instances === --

-- Primitive

type instance Target (Ref (Edge (Arc src tgt))) = Ref (Node tgt)
type instance Source (Ref (Edge (Arc src tgt))) = Ref (Node src)

-- Connections

type instance Connection (Ref  a) (Ref  b) = Ref (Connection a b)
type instance Connection (Node a) (Node b) = Edge $ Arc a b

-- Conversions

instance (Castable (Ref src) (Ref src'), Castable (Ref tgt) (Ref tgt')) => Castable (Arc src tgt) (Arc src' tgt') where
    cast (Arc src tgt) = Arc (cast src) (cast tgt) ; {-# INLINE cast #-}

