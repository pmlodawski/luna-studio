module Luna.Syntax.Model.Graph.Cluster where

import Prelude.Luna

import Luna.Syntax.Model.Graph.Ref
import Luna.Syntax.Model.Graph.Class

import Data.Container
import Data.Index
import Data.Prop

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet


-- === Definitions === --

--TODO[WD]: Refactor from Graph.hs


-- === Utils === --

add :: Int -> Cluster -> Cluster
add el = wrapped %~ IntSet.insert el

remove :: Int -> Cluster -> Cluster
remove el = wrapped %~ IntSet.delete el

member :: Int -> Cluster -> Bool
member el = IntSet.member el ∘ unwrap'


-- === Instances === --

-- Wrappers
makeWrapped ''Cluster


-- Properties

--type instance            Prop p (Node t) = Prop p t
--instance Getter a t => Getter a (Node t) where getter a = getter a ∘ unwrap'      ; {-# INLINE getter #-}
--instance Setter a t => Setter a (Node t) where setter   = over wrapped' ∘∘ setter ; {-# INLINE setter #-}

instance Getter (Ref Cluster) (Graph n e) where getter ref     = index_ (ref ^. idx) ∘ view clusters                    ; {-# INLINE getter #-}
instance Setter (Ref Cluster) (Graph n e) where setter ref val = clusters %~ unchecked inplace insert_ (ref ^. idx) val ; {-# INLINE setter #-}
