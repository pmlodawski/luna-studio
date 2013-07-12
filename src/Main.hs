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

import           Luna.DefManager  (DefManager)
import qualified Luna.Graph as Graph
import           Luna.Graph   (Graph)
import qualified Luna.Node as Node
import           Luna.Node   (Node)
import qualified Luna.NodeDef as NodeDef
import qualified Luna.Samples as Samples
import qualified Data.GraphViz as GV
import qualified Luna.CodeGenerator as CG

--import Text.Show.Pretty
--import Text.Groom

defaultVis :: (DG.Graph gr) => gr nl el -> GV.DotGraph DG.Node
defaultVis = GV.graphToDot GV.nonClusteredParams

main :: IO ()
main = do 
	let 
		(node, manager) = Samples.sample_helloWorld
		nodeDef = Node.def node
		graph = NodeDef.graph nodeDef
	showGraph graph
	showCode node manager
	return ()

showCode :: Node -> DefManager -> IO ()
showCode node manager = putStrLn $ CG.generateCode node manager

showGraph :: Graph -> IO ()
showGraph graph = do 
	let 
		graphrepr = Graph.repr graph
	print $ defaultVis graphrepr  -- prints dot graphviz representation
	GV.preview $ graphrepr-- shows interactive view while compiling from sublime

	return ()

