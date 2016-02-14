{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Luna.Syntax.Model.Graph.Class where

import Prologue hiding (Getter, Setter, read)

import Control.Monad.Event
import Data.Prop
import Data.Construction
import Data.Container
import Data.Container.Auto      (Auto)
import Data.Container.Resizable (Exponential)
import Data.Index
import Data.Vector              (Vector)
import Data.IntSet              (IntSet)

import qualified Data.IntSet as IntSet

import Data.Graph.Referenced

import Data.Graph.Backend.Vector


-----------------
-- TO REFACTOR --
-----------------

instance Castable n node => Getter (Ref (Node node)) (VectorGraph n e) where getter ref     = Node ∘ cast ∘ index_ (ref ^. idx) ∘ view nodeGraph                       ; {-# INLINE getter #-}
instance Castable node n => Setter (Ref (Node node)) (VectorGraph n e) where setter ref val = nodeGraph %~ unchecked inplace insert_ (ref ^. idx) (cast $ unwrap' val) ; {-# INLINE setter #-}

instance Castable e edge => Getter (Ref (Edge edge)) (VectorGraph n e) where getter ref     = Edge ∘ cast ∘ index_ (ref ^. idx) ∘ view edgeGraph                       ; {-# INLINE getter #-}
instance Castable edge e => Setter (Ref (Edge edge)) (VectorGraph n e) where setter ref val = edgeGraph %~ unchecked inplace insert_ (ref ^. idx) (cast $ unwrap' val) ; {-# INLINE setter #-}




----------------------------
-- TO DELETE AND REFACTOR --
----------------------------

-- Ref accessors
type instance Prop (Ref a) (VectorGraph n e) = a
