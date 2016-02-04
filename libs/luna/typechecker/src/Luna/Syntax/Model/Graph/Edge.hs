{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Graph.Edge where

import Prelude.Luna

import Luna.Syntax.Model.Graph.Class
import Luna.Syntax.Model.Graph.Ref

import Data.Attribute
import Control.Monad.Event


-- === Definitions === --

newtype Node       a = Node a                   deriving (Show, Eq, Ord, Functor, Traversable, Foldable)
data    Edge src tgt = Edge (Ref src) (Ref tgt) deriving (Show, Eq, Ord)
type    Link       a = Edge a a

type family Connection  src dst
type family Conn_Source a -- TODO[WD]: change name after refactoring
type family Conn_Target a -- TODO[WD]: change name after refactoring

class ( src ~ Conn_Source (Connection src tgt)
      , tgt ~ Conn_Target (Connection src tgt)
      ) => Connectible src tgt m where connection :: src -> tgt -> m (Connection src tgt)

-- Properties
data Inputs  = Inputs  deriving (Show)
data Outputs = Outputs deriving (Show)


-- === Utils === --

edge :: Ref (Node src) -> Ref (Node tgt) -> Edge src tgt
edge src tgt = Edge (rewrap src) (rewrap tgt)

source :: Lens' (Edge src tgt) (Ref (Node src))
source = lens (\(Edge src _) -> rewrap src) (\(Edge _ tgt) src -> Edge (rewrap src) tgt)

target :: Lens' (Edge src tgt) (Ref (Node tgt))
target = lens (\(Edge _ tgt) -> rewrap tgt) (\(Edge src _) tgt -> Edge src (rewrap tgt))


-- === Instances === --

-- Primitive

type instance Conn_Source (Ref a) = Ref (Conn_Source a)
type instance Conn_Target (Ref a) = Ref (Conn_Target a)
type instance Conn_Source (Edge src tgt) = Node src
type instance Conn_Target (Edge src tgt) = Node tgt

-- Wrappers

makeWrapped ''Node
type instance Unlayered (Node a) = (Unwrapped (Node a))
instance      Layered   (Node a)

-- Connecting

type instance Connection (Ref  a) (Ref  b) = Ref (Connection a b)
type instance Connection (Node a) (Node b) = Edge a b

instance (LayerConstructor m c, Dispatcher CONNECTION c m, Unlayered c ~ Edge src tgt, c ~ Connection (Ref (Node src)) (Ref (Node tgt))) 
      => Connectible (Ref (Node src)) (Ref (Node tgt)) m where
         connection src tgt = dispatch CONNECTION =<< constructLayer (edge src tgt)

-- Construction

instance Monad m => LayerConstructor m (Node a) where constructLayer = return ∘ Node    ; {-# INLINE constructLayer #-}
instance Monad m => LayerDestructor  m (Node a) where destructLayer  = return ∘ unwrap' ; {-# INLINE destructLayer #-}

-- Conversions

instance (Castable (Ref src) (Ref src'), Castable (Ref tgt) (Ref tgt')) => Castable (Edge src tgt) (Edge src' tgt') where 
    cast (Edge src tgt) = Edge (cast src) (cast tgt) ; {-# INLINE cast #-}

instance Castable a a' => Castable (Node a) (Node a') where
    cast = wrapped %~ cast

-- Attributes

type instance            Attr a (Node t) = Attr a t
instance Getter a t => Getter a (Node t) where getter a = getter a ∘ unwrap'      ; {-# INLINE getter #-}
instance Setter a t => Setter a (Node t) where setter   = over wrapped' ∘∘ setter ; {-# INLINE setter #-}
