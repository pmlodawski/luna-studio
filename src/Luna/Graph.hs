---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.Graph(
Graph(..),
empty,

insNode, insNodes,
delNode, delNodes,
insEdge, insEdges,
delEdge, delEdges,

nodeById,

typeByName, callByName, classByName, packageByName, functionsByName

) where

import qualified Data.Graph.Inductive as DG
import qualified Data.Map as Map
import qualified Data.MultiMap as MultiMap
import qualified Luna.Node as Node
import           Luna.Node (Node)
import           Luna.Edge (Edge)
import Luna.Common(Graph(..))


empty :: Graph
empty = Graph DG.empty Map.empty Map.empty Map.empty MultiMap.empty Map.empty


doMulti function elements graph = foldr function graph elements


insNodes :: [DG.LNode Node] -> Graph -> Graph
insNodes = doMulti insNode

insNode :: DG.LNode Node -> Graph -> Graph
insNode lnode@(id_, node) graph =
	let 
		newgraph = graph{repr=DG.insNode lnode $ repr graph}
		updateNodeMap 	   = Map.insert      (Node.name node) id_
		updateNodeMultiMap = MultiMap.insert (Node.name node) id_
	in case node of
		Node.TypeNode     _   -> newgraph{types  	  = updateNodeMap      $ types graph}
		Node.CallNode     _   -> newgraph{calls  	  = updateNodeMap      $ calls graph}
		Node.ClassNode    _ _ -> newgraph{classes	  = updateNodeMap      $ classes graph}
		Node.FunctionNode _ _ -> newgraph{functions = updateNodeMultiMap $ functions graph}
		Node.PackageNode  _ _ -> newgraph{packages  = updateNodeMap      $ packages graph}
		_                     -> newgraph

delNodes :: [DG.Node] -> Graph -> Graph
delNodes = doMulti delNode

delNode :: DG.Node -> Graph -> Graph
delNode id_ graph =
	let
		newgraph = graph{repr=DG.delNode id_ $ repr graph}
		(_, node)     = DG.labNode' $ DG.context (repr graph) id_
		updateNodeMap 	   = Map.delete      (Node.name node)
		updateNodeMultiMap = MultiMap.delete (Node.name node)
	in case node of
		Node.TypeNode     _   -> newgraph{types  	  = updateNodeMap      $ types graph}
		Node.CallNode     _   -> newgraph{calls  	  = updateNodeMap      $ calls graph}
		Node.ClassNode    _ _ -> newgraph{classes	  = updateNodeMap      $ classes graph}
		Node.FunctionNode _ _ -> newgraph{functions = updateNodeMultiMap $ functions graph}
		Node.PackageNode  _ _ -> newgraph{packages  = updateNodeMap      $ packages graph}
		_                     -> newgraph


insEdges :: [DG.LEdge Edge] -> Graph -> Graph
insEdges = doMulti insEdge 

insEdge :: DG.LEdge Edge -> Graph -> Graph
insEdge ledge graph = graph{repr = DG.insEdge ledge $ repr graph}


delEdges :: [DG.Edge] -> Graph -> Graph
delEdges = doMulti delEdge 

delEdge :: DG.Edge -> Graph -> Graph
delEdge edge graph = graph{repr = DG.delEdge edge $ repr graph}


nodeById :: DG.Node -> Graph -> Node
nodeById id_ graph = let
	(_, node) = DG.labNode' $ DG.context (repr graph) id_
	in node

nodeByName :: Ord k => (Graph -> Map.Map k DG.Node) -> k -> Graph -> Maybe Node
nodeByName getter name graph = 
	case Map.lookup name $ getter graph of
	 	Just id_ -> Just(nodeById id_ graph)
	 	Nothing  -> Nothing

nodesByName :: Ord k => (Graph -> MultiMap.MultiMap k DG.Node) -> k -> Graph -> [Node]
nodesByName getter name graph = [nodeById elid graph | elid <- ids] where
	ids = MultiMap.lookup name $ getter graph
	
typeByName :: String -> Graph -> Maybe Node
typeByName 		= nodeByName  types

callByName :: String -> Graph -> Maybe Node
callByName 		= nodeByName  calls

classByName :: String -> Graph -> Maybe Node
classByName 	= nodeByName  classes

packageByName :: String -> Graph -> Maybe Node
packageByName 	= nodeByName  packages

functionsByName :: String -> Graph -> [Node]
functionsByName = nodesByName functions

