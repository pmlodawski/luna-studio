{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Referenced.Edge where

import Prelude.Luna

import Data.Graph.Referenced.Ref
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

--data Edge src tgt = Edge (Ref src) (Ref tgt) deriving (Show, Eq, Ord)
type Link       a = Arc a a

newtype Arrow tgt     = Arrow (Ref tgt)           deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data    Arc   src tgt = Arc   (Ref src) (Ref tgt) deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
--newtype Link  t       = Link  (Ref t)             deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

type family Connection src tgt


-- === Utils === --

--edge ::


edge :: Ref (Node src) -> Ref (Node tgt) -> Arc src tgt
edge src tgt = Arc (rewrap src) (rewrap tgt)

source :: Lens' (Arc src tgt) (Ref (Node src))
source = lens (\(Arc src _) -> rewrap src) (\(Arc _ tgt) src -> Arc (rewrap src) tgt)

target :: Lens' (Arc src tgt) (Ref (Node tgt))
target = lens (\(Arc _ tgt) -> rewrap tgt) (\(Arc src _) tgt -> Arc src (rewrap tgt))


-- === Instances === --

-- Primitive

type instance Target (Ref (Arc src tgt)) = Ref (Node tgt)
type instance Source (Ref (Arc src tgt)) = Ref (Node src)

-- Connections

type instance Connection (Ref  a) (Ref  b) = Ref (Connection a b)
type instance Connection (Node a) (Node b) = Arc a b

-- Conversions

instance (Castable (Ref src) (Ref src'), Castable (Ref tgt) (Ref tgt')) => Castable (Arc src tgt) (Arc src' tgt') where
    cast (Arc src tgt) = Arc (cast src) (cast tgt) ; {-# INLINE cast #-}

