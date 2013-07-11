---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Graph(
Graph,
Node,
empty
) where

import qualified Data.Graph.Inductive as DG
import Data.GraphViz.Attributes (Labellable, toLabelValue)

import qualified Luna.DefaultValue as DefaultValue
import qualified Luna.NodeDef as NodeDef
import qualified Luna.Edge as Edge

type Graph = DG.Gr Node Edge.Edge

empty :: Graph
empty = DG.empty 

data Node = TypeNode    { name :: String }
		  | CallNode    { name :: String }
		  | ClassNode   { name :: String, graph :: Graph, def :: NodeDef.NodeDef }
		  | FunctionNode{ name :: String, graph :: Graph, def :: NodeDef.NodeDef }
		  | PackageNode { name :: String, graph :: Graph, def :: NodeDef.NodeDef }
		  | DefaultNode { defValue :: DefaultValue.DefaultValue }
		  deriving (Show)

instance Labellable Node where
	toLabelValue = toLabelValue . show

