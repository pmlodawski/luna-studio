---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Graph(
--Common.Graph(..)
--empty
) where

import qualified Data.Graph.Inductive as DG
import qualified Data.Map as Map
import qualified Data.MultiMap as MultiMap
import qualified Luna.Node as Node
import qualified Luna.Edge as Edge
import qualified Luna.Common as G



insNode lnode@(id, node) graph =
	let 
		newgraph = graph{G.repr=DG.insNode lnode $ G.repr graph}
		updateNodeMap 	   = Map.insert      (Node.name node) id
		updateNodeMultiMap = MultiMap.insert (Node.name node) id
	in case node of
		Node.TypeNode     _   -> newgraph{G.types  	  = updateNodeMap      $ G.types graph}
		Node.CallNode     _   -> newgraph{G.calls  	  = updateNodeMap      $ G.calls graph}
		Node.ClassNode    _ _ -> newgraph{G.classes	  = updateNodeMap      $ G.classes graph}
		Node.FunctionNode _ _ -> newgraph{G.functions = updateNodeMultiMap $ G.functions graph}
		Node.PackageNode  _ _ -> newgraph{G.packages  = updateNodeMap      $ G.packages graph}
		_                     -> newgraph

delNode :: DG.Node -> G.Graph -> G.Graph
delNode id graph =
	let
		newgraph = graph{G.repr=DG.delNode id $ G.repr graph}
		(_, node)     = DG.labNode' $ DG.context (G.repr graph) id
		updateNodeMap 	   = Map.delete      (Node.name node)
		updateNodeMultiMap = MultiMap.delete (Node.name node)
	in case node of
		Node.TypeNode     _   -> newgraph{G.types  	  = updateNodeMap      $ G.types graph}
		Node.CallNode     _   -> newgraph{G.calls  	  = updateNodeMap      $ G.calls graph}
		Node.ClassNode    _ _ -> newgraph{G.classes	  = updateNodeMap      $ G.classes graph}
		Node.FunctionNode _ _ -> newgraph{G.functions = updateNodeMultiMap $ G.functions graph}
		Node.PackageNode  _ _ -> newgraph{G.packages  = updateNodeMap      $ G.packages graph}
		_                     -> newgraph

insEdge :: DG.LEdge Edge.Edge-> G.Graph -> G.Graph
insEdge ledge graph = graph{G.repr = DG.insEdge ledge $ G.repr graph}

delEdge :: DG.Edge -> G.Graph -> G.Graph
delEdge edge graph = graph{G.repr = DG.delEdge edge $ G.repr graph}


node id graph = let
	(_, node) = DG.labNode' $ DG.context (G.repr graph) id
	in node


typeByName name graph = 
	case Map.lookup name $ G.types graph of
	 	Just id -> Just(node id graph)
	 	Nothing -> Nothing


--Map.lookup "Vector" $ Graph.types g