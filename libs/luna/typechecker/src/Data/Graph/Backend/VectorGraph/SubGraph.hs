module Data.Graph.Backend.VectorGraph.SubGraph where

import Prologue

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet


-- === Definitions === --

newtype SubGraph = SubGraph IntSet deriving (Show)
makeLenses  ''SubGraph
makeWrapped ''SubGraph


-- === Utils === --

add :: Int -> SubGraph -> SubGraph
add el = wrapped' %~ IntSet.insert el

remove :: Int -> SubGraph -> SubGraph
remove el = wrapped' %~ IntSet.delete el

member :: Int -> SubGraph -> Bool
member el = IntSet.member el ∘ unwrap'


-- === Instances === --

-- Cast

instance Castable SubGraph SubGraph where cast = id

---- Wrappers
--makeWrapped ''Cluster


---- Properties

----instance Getter (Ref Cluster) (VectorGraph n e) where getter ref     = index_ (ref ^. idx) ∘ view clusterGraph                    ; {-# INLINE getter #-}
----instance Setter (Ref Cluster) (VectorGraph n e) where setter ref val = clusterGraph %~ unchecked inplace insert_ (ref ^. idx) val ; {-# INLINE setter #-}

