---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

import qualified Data.Graph.Inductive as DG
--import Control.Monad.State
--import Data.Graph.Inductive.Tree
--import Data.Graph.Inductive.Monad
--import Data.Graph.Inductive.Monad.IOArray

import qualified Luna
import qualified Luna.Edge as Edge
import qualified Luna.DefManager as DefManager
import qualified Luna.Graph as Graph
import qualified Luna.NodeDef as NodeDef
import qualified Luna.GraphSerialization as GS

import qualified Data.GraphViz as GV



--import Text.Show.Pretty
--import Text.Groom

defaultVis :: (DG.Graph gr) => gr nl el -> GV.DotGraph DG.Node
defaultVis = GV.graphToDot GV.nonClusteredParams

main :: IO ()
main = do 
	showGraph example_helloWorld
	return ()
	--let
	--	d  = DefTree.insert ["std","math"] (Luna.Package $ NodeDef.NodeDef "std" Graph.empty NodeDef.noPorts NodeDef.noPorts) $ 
	--		 DefTree.insert ["std"] (Luna.Package $ NodeDef.NodeDef "std" Graph.empty NodeDef.noPorts NodeDef.noPorts) $
	--		 DefTree.empty
	--print d
	--return ()


--example_1 :: Luna.Graph
--example_1 = let
--	manager = DefManager.empty

--	g :: Luna.Graph
--	g = DG.empty
--	g2 = DG.insNodes (zip [1..] [Luna.Node "ala", Luna.Node "bob"]) g
--	g3 = DG.insEdges [(1,2,Luna.Edge "a" "b" Edge.Standard)] g2
--	in g3

showGraph :: (Graph.Graph, DefManager.DefManager) -> IO()
showGraph (graph, manager) = do 
	print graph
	print manager
	print $ defaultVis graph -- prints dot graphviz representation
	GV.preview graph -- shows interactive view while compiling from sublime
	return ()

example_helloWorld :: (DG.Gr Luna.Node Edge.Edge, DefManager.DefManager)
example_helloWorld = (graph, manager) where
	manager = DefManager.insert ["std", "io", "Console", "print"]  (Luna.Class    $ NodeDef.NodeDef "Console" Graph.empty ["self", "value"] ["console"]) 
			$ DefManager.insert ["std", "io", "Console", "init"]   (Luna.Function $ NodeDef.NodeDef "init" 	  Graph.empty ["self"]   		["instance"])
			$ DefManager.insert ["std", "io", "Console"] 		   (Luna.Class    $ NodeDef.NodeDef "Console" Graph.empty NodeDef.noPorts   NodeDef.noPorts) 
			$ DefManager.insert ["std", "io"] 					   (Luna.Package  $ NodeDef.NodeDef "io"      Graph.empty NodeDef.noPorts   NodeDef.noPorts) 
			$ DefManager.insert ["std", "types", "String", "init"] (Luna.Function $ NodeDef.NodeDef "init"    Graph.empty ["self", "value"] ["instance"])    
			$ DefManager.insert ["std", "types", "String"] 		   (Luna.Class    $ NodeDef.NodeDef "String"  Graph.empty NodeDef.noPorts   NodeDef.noPorts) 
			$ DefManager.insert ["std", "types", "new"] 		   (Luna.Function $ NodeDef.NodeDef "new"     Graph.empty ["type"]          ["instance"])    
			$ DefManager.insert ["std", "types", "type"] 		   (Luna.Function $ NodeDef.NodeDef "type"    Graph.empty ["name"]          ["type"])        
			$ DefManager.insert ["std", "types"] 				   (Luna.Package  $ NodeDef.NodeDef "types"   Graph.empty NodeDef.noPorts   NodeDef.noPorts) 
			$ DefManager.insert ["std"] 						   (Luna.Package  $ NodeDef.NodeDef "std"     Graph.empty NodeDef.noPorts   NodeDef.noPorts) 
			$ DefManager.empty

	graph   = DG.insEdges [ (0,1,Luna.Edge "value" "name" Edge.Standard),
							(1,2,Luna.Edge "type" "type" Edge.Standard),
							(2,3,Luna.Edge "instance" "self" Edge.Standard),
							(4,5,Luna.Edge "value" "name" Edge.Standard),
							(5,6,Luna.Edge "type" "type" Edge.Standard),
							(6,8,Luna.Edge "instance" "self" Edge.Standard),
							(7,8,Luna.Edge "value" "value" Edge.Standard),
							(3,9,Luna.Edge "instance" "self" Edge.Standard),
							(8,9,Luna.Edge "instance" "value" Edge.Standard)
			  			  ]
			$ DG.insNodes [ (0, Luna.DefaultNode $ Luna.DefaultString "std.types.Console"),
							(1, Luna.Node "std.types.type"),
							(2, Luna.Node "std.types.new"),
							(3, Luna.Node "std.types.Console.init"),
							(4, Luna.DefaultNode $ Luna.DefaultString "std.types.String"),
							(5, Luna.Node "std.types.type"),
							(6, Luna.Node "std.types.new"),
							(7, Luna.DefaultNode $ Luna.DefaultString "hello world!"),
							(8, Luna.Node "std.types.String.init"),
							(9, Luna.Node "std.types.Console.print")
						  ]
		  	$ Graph.empty





--insN :: [Node] -> State Graph ()
--insN n = State $ \s -> DG.insNodes n g

--insE :: [Edge] -> State Graph ()
--insE e = State $ \s -> DG.insEdges e g

--graphManip :: State Graph Graph
--graphManip = do
--	insN (zip [1..] [Node "ala", Node "bob"])
--	insE [(1,2,Edge "a" "b" Standard)]


