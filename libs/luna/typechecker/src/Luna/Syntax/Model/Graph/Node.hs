{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Graph.Node where

import Prelude.Luna

import Luna.Syntax.Model.Graph.Ref

import Data.Container
import Data.Index
import Data.Prop
import Luna.Syntax.Model.Graph.Class


-- === Definitions === --

newtype Node a = Node a deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

data Inputs  = Inputs  deriving (Show)
data Outputs = Outputs deriving (Show)


-- === Instances === --

-- Wrappers

makeWrapped ''Node
type instance Uncovered (Node a) = Uncovered (Unlayered (Node a))
type instance Unlayered (Node a) = Unwrapped (Node a)
instance      Layered   (Node a)


-- Construction

instance Monad m => LayerConstructor m (Node a) where constructLayer = return ∘ Node    ; {-# INLINE constructLayer #-}
instance Monad m => LayerDestructor  m (Node a) where destructLayer  = return ∘ unwrap' ; {-# INLINE destructLayer  #-}

-- Conversions

instance Castable a a' => Castable (Node a) (Node a') where
    cast = wrapped %~ cast ; {-# INLINE cast #-}

-- Properties

type instance            Prop p (Node t) = Prop p t
instance Getter a t => Getter a (Node t) where getter a = getter a ∘ unwrap'      ; {-# INLINE getter #-}
instance Setter a t => Setter a (Node t) where setter   = over wrapped' ∘∘ setter ; {-# INLINE setter #-}

instance Castable n node => Getter (Ref (Node node)) (Graph n e) where getter ref     = Node ∘ cast ∘ index_ (ref ^. idx) ∘ view nodeGraph                       ; {-# INLINE getter #-}
instance Castable node n => Setter (Ref (Node node)) (Graph n e) where setter ref val = nodeGraph %~ unchecked inplace insert_ (ref ^. idx) (cast $ unwrap' val) ; {-# INLINE setter #-}
