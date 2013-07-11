---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Graph(
Graph(..),
insNode,
delNode,
insEdge,
delEdge,
node,
typeByName,
callByName,
classByName,
packageByName,
functionsByName
) where

import qualified Data.Graph.Inductive as DG
import qualified Data.Map as Map
import qualified Data.MultiMap as MultiMap
import qualified Luna.Node as Node
import qualified Luna.Edge as Edge
import Luna.Common(Graph(..))

empty :: Graph
empty = Graph DG.empty Map.empty Map.empty Map.empty MultiMap.empty Map.empty

insNode :: DG.LNode Node.Node -> Graph -> Graph
insNode lnode@(id, node) graph =
	let 
		newgraph = graph{repr=DG.insNode lnode $ repr graph}
		updateNodeMap 	   = Map.insert      (Node.name node) id
		updateNodeMultiMap = MultiMap.insert (Node.name node) id
	in case node of
		Node.TypeNode     _   -> newgraph{types  	  = updateNodeMap      $ types graph}
		Node.CallNode     _   -> newgraph{calls  	  = updateNodeMap      $ calls graph}
		Node.ClassNode    _ _ -> newgraph{classes	  = updateNodeMap      $ classes graph}
		Node.FunctionNode _ _ -> newgraph{functions = updateNodeMultiMap $ functions graph}
		Node.PackageNode  _ _ -> newgraph{packages  = updateNodeMap      $ packages graph}
		_                     -> newgraph

delNode :: DG.Node -> Graph -> Graph
delNode id graph =
	let
		newgraph = graph{repr=DG.delNode id $ repr graph}
		(_, node)     = DG.labNode' $ DG.context (repr graph) id
		updateNodeMap 	   = Map.delete      (Node.name node)
		updateNodeMultiMap = MultiMap.delete (Node.name node)
	in case node of
		Node.TypeNode     _   -> newgraph{types  	  = updateNodeMap      $ types graph}
		Node.CallNode     _   -> newgraph{calls  	  = updateNodeMap      $ calls graph}
		Node.ClassNode    _ _ -> newgraph{classes	  = updateNodeMap      $ classes graph}
		Node.FunctionNode _ _ -> newgraph{functions = updateNodeMultiMap $ functions graph}
		Node.PackageNode  _ _ -> newgraph{packages  = updateNodeMap      $ packages graph}
		_                     -> newgraph

insEdge :: DG.LEdge Edge.Edge-> Graph -> Graph
insEdge ledge graph = graph{repr = DG.insEdge ledge $ repr graph}

delEdge :: DG.Edge -> Graph -> Graph
delEdge edge graph = graph{repr = DG.delEdge edge $ repr graph}

node :: DG.Node -> Graph -> Node.Node
node id graph = let
	(_, node) = DG.labNode' $ DG.context (repr graph) id
	in node


nodeByName getter name graph = 
	case Map.lookup name $ getter graph of
	 	Just id -> Just(node id graph)
	 	Nothing -> Nothing

nodesByName getter name graph = 
	case MultiMap.lookup name $ getter graph of
	 	ids@[id] -> [node id graph | id <- ids]
	 	[] -> []


typeByName 		= nodeByName  types
callByName 		= nodeByName  calls
classByName 	= nodeByName  classes
packageByName 	= nodeByName  packages
functionsByName = nodesByName functions

