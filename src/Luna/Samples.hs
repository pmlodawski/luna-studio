module Luna.Samples(
sample_helloWorld
) where

import qualified Data.Graph.Inductive as DG

import qualified Luna
import qualified Luna.Edge as Edge
import qualified Luna.DefManager as DefManager
import qualified Luna.Graph as Graph
import qualified Luna.NodeDef as NodeDef

sample_helloWorld :: (DG.Gr Luna.Node Edge.Edge, DefManager.DefManager)
sample_helloWorld = (graph, manager) where

	consoleGraph = DG.insNodes [(0, Luna.Node "std.io.Console.init"),
								(1, Luna.Node "std.io.Console.print")]
				 $ Graph.empty

	stringGraph  = DG.insNodes [(0, Luna.Node "std.types.String.init")]
				 $ Graph.empty	

	typesGraph   = DG.insNodes [(0, Luna.Node "std.types.String"),
								(1, Luna.Node "std.types.new"),
								(2, Luna.Node "std.types.type")]
				 $ Graph.empty

	stdGraph     = DG.insNodes [(0, Luna.Node "std.io"),
								(1, Luna.Node "std.types")]
				 $ Graph.empty

	manager = DefManager.insert ["std", "io", "Console", "print"]  (Luna.Class    $ NodeDef.NodeDef "Console" Graph.empty  ["self", "value"] ["console"]) 
			$ DefManager.insert ["std", "io", "Console", "init"]   (Luna.Function $ NodeDef.NodeDef "init" 	  Graph.empty  ["self"]   		["instance"])
			$ DefManager.insert ["std", "io", "Console"] 		   (Luna.Class    $ NodeDef.NodeDef "Console" consoleGraph NodeDef.noPorts   NodeDef.noPorts) 
			$ DefManager.insert ["std", "io"] 					   (Luna.Package  $ NodeDef.NodeDef "io"      Graph.empty  NodeDef.noPorts   NodeDef.noPorts) 
			$ DefManager.insert ["std", "types", "String", "init"] (Luna.Function $ NodeDef.NodeDef "init"    Graph.empty  ["self", "value"] ["instance"])    
			$ DefManager.insert ["std", "types", "String"] 		   (Luna.Class    $ NodeDef.NodeDef "String"  stringGraph  NodeDef.noPorts   NodeDef.noPorts) 
			$ DefManager.insert ["std", "types", "new"] 		   (Luna.Function $ NodeDef.NodeDef "new"     Graph.empty  ["type"]          ["instance"])    
			$ DefManager.insert ["std", "types", "type"] 		   (Luna.Function $ NodeDef.NodeDef "type"    Graph.empty  ["name"]          ["type"])        
			$ DefManager.insert ["std", "types"] 				   (Luna.Package  $ NodeDef.NodeDef "types"   typesGraph   NodeDef.noPorts   NodeDef.noPorts) 
			$ DefManager.insert ["std"] 						   (Luna.Package  $ NodeDef.NodeDef "std"     stdGraph     NodeDef.noPorts   NodeDef.noPorts) 
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
