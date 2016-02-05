{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Graph.Edge where

import Prelude.Luna

import Luna.Syntax.Model.Graph.Class
import Luna.Syntax.Model.Graph.Ref
import Luna.Syntax.Model.Graph.Node

import Data.Prop
import Control.Monad.Event
import Data.Direction
import Data.Index
import Data.Container


------------------
-- === Edge === --
------------------

-- === Definitions === --

type family Connection src dst

data Edge src dst = Edge (Ref src) (Ref dst) deriving (Show, Eq, Ord)
type Link       a = Edge a a

class ( src ~ Source (Connection src dst)
      , dst ~ Target (Connection src dst)
      ) => Connectible src dst m where connection :: src -> dst -> m (Connection src dst)

class conn ~ Connection src dst => Connectible2 src dst m conn | src dst -> conn, conn -> src dst where
    connection2 ::  src -> dst -> m conn

-- === Utils === --

edge :: Ref (Node src) -> Ref (Node dst) -> Edge src dst
edge src dst = Edge (rewrap src) (rewrap dst)

source :: Lens' (Edge src dst) (Ref (Node src))
source = lens (\(Edge src _) -> rewrap src) (\(Edge _ dst) src -> Edge (rewrap src) dst)

target :: Lens' (Edge src dst) (Ref (Node dst))
target = lens (\(Edge _ dst) -> rewrap dst) (\(Edge src _) dst -> Edge src (rewrap dst))


-- === Instances === --

-- Primitive

type instance Target (Ref (Edge src dst)) = Ref (Node dst)
type instance Source (Ref (Edge src dst)) = Ref (Node src)

-- Connecting

type instance Connection (Ref  a) (Ref  b) = Ref (Connection a b)
type instance Connection (Node a) (Node b) = Edge a b

instance (LayerConstructor m c, Dispatcher CONNECTION c m, Unlayered c ~ Edge src dst, c ~ Connection (Ref (Node src)) (Ref (Node dst)))
      => Connectible (Ref (Node src)) (Ref (Node dst)) m where
         connection src dst = dispatch CONNECTION =<< constructLayer (edge src dst)

-- Conversions

instance (Castable (Ref src) (Ref src'), Castable (Ref dst) (Ref dst')) => Castable (Edge src dst) (Edge src' dst') where
    cast (Edge src dst) = Edge (cast src) (cast dst) ; {-# INLINE cast #-}

-- Properties

instance Castable e (Edge src tgt) => Getter (Ref (Edge src tgt)) (Graph n e) where getter ref     = cast ∘ index_ (ref ^. idx) ∘ view edgeGraph                    ; {-# INLINE getter #-}
instance Castable (Edge src tgt) e => Setter (Ref (Edge src tgt)) (Graph n e) where setter ref val = edgeGraph %~ unchecked inplace insert_ (ref ^. idx) (cast val) ; {-# INLINE setter #-}
