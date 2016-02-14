module Data.Graph.Model.Class where

import Prologue

import Data.Graph.Model.Edge
import Data.Graph.Model.Node
import Data.Graph.Model.Ref


-- === Attributes === --

data Inputs  = Inputs  deriving (Show)
data Outputs = Outputs deriving (Show)

data ELEMENT    = ELEMENT    deriving (Show)
data CONNECTION = CONNECTION deriving (Show)


-- === Definitions === ---

class Graph g where
    nodes :: Lens' g [Node (Item g)]
    edges :: Lens' g [Link (Item g)]

class Referrenced g where
    nodeRefs :: g -> [Ref $ Node (Item g)]
    edgeRefs :: g -> [Ref $ Link (Item g)]

