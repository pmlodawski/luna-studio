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

newtype Arrow tgt     = Arrow (Ref Node tgt)                deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data    Arc   src tgt = Arc   (Ref Node src) (Ref Node tgt) deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
type    Link  a       = Arc   a a

type family Connection src tgt


-- === Utils === --

--class HasSource a where

arc :: Ref Node src -> Ref Node tgt -> Arc src tgt
arc = Arc

link :: Ref Node t -> Ref Node t -> Link t
link = arc

arrow :: Ref Node tgt -> Arrow tgt
arrow = Arrow

--edge :: Ref Node src -> Ref Node tgt -> Edge $ Arc src tgt
--edge = Edge ∘ arc


-- FIXME[WD]: refactor source / target to use the Data.Direction abstraction

--source :: Lens' (Arc src tgt) (Ref Node src)
--source = lens (\(Arc src _) -> src) (\(Arc _ tgt) src -> Arc src tgt)

--target :: Lens' (Arc src tgt) (Ref Node tgt)
--target = lens (\(Arc _ tgt) -> tgt) (\(Arc src _) tgt -> Arc src tgt)


-- === Instances === --

-- Functors

instance Bifunctor Arc where bimap f g (Arc src tgt) = Arc (f <$> src) (g <$> tgt) ; {-# INLINE bimap #-}

-- Directions


type instance Prop Target (Ref Edge a) = Ref Node (a # Target)
type instance Prop Source (Ref Edge a) = Ref Node (a # Source)

--type instance Prop Target (Arc src tgt) = tgt
--type instance Prop Source (Arc src tgt) = src


type instance Prop Target (Arc src tgt) = Ref Node tgt
type instance Prop Source (Arc src tgt) = Ref Node src
instance      HasSource (Arc src tgt) where source = lens (\(Arc src _) -> src) (\(Arc _ tgt) src -> Arc src tgt)
instance      HasTarget (Arc src tgt) where target = lens (\(Arc _ tgt) -> tgt) (\(Arc src _) tgt -> Arc src tgt)

type instance Prop Target (Arrow tgt) = (Ref Node tgt)

-- Connections

type instance Connection (Ref Node a) (Ref Node b) = Arc a b

-- Conversions

instance (Castable src src', Castable tgt tgt') => Castable (Arc src tgt) (Arc src' tgt') where
    cast (Arc src tgt) = Arc (cast src) (cast tgt) ; {-# INLINE cast #-}

