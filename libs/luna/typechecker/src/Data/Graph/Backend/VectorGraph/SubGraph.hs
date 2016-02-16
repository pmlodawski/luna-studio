module Data.Graph.Backend.VectorGraph.SubGraph where

import Prologue

import           Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet


-- === Definitions === --

data SubGraph = SubGraph { _members :: IntSet
                         , _label   :: String
                         } deriving (Show)
makeLenses  ''SubGraph

-- === Utils === --

add :: Int -> SubGraph -> SubGraph
add el = members %~ IntSet.insert el

remove :: Int -> SubGraph -> SubGraph
remove el = members %~ IntSet.delete el

member :: Int -> SubGraph -> Bool
member el = IntSet.member el ∘ view members

nodes :: SubGraph -> [Int]
nodes = IntSet.toList . view members


-- === Instances === --

-- Cast

instance Castable SubGraph SubGraph where cast = id

---- Wrappers
--makeWrapped ''Cluster


---- Properties

----instance Getter (Ref Cluster) (VectorGraph n e) where getter ref     = index_ (ref ^. idx) ∘ view clusterGraph                    ; {-# INLINE getter #-}
----instance Setter (Ref Cluster) (VectorGraph n e) where setter ref val = clusterGraph %~ unchecked inplace insert_ (ref ^. idx) val ; {-# INLINE setter #-}

