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


import qualified Luna.DefManager as DefManager
import qualified Luna.Graph as Graph
import qualified Luna.GraphSerialization as GS
import qualified Luna.Samples as Samples
import qualified Data.GraphViz as GV



--import Text.Show.Pretty
--import Text.Groom

defaultVis :: (DG.Graph gr) => gr nl el -> GV.DotGraph DG.Node
defaultVis = GV.graphToDot GV.nonClusteredParams

main :: IO ()
main = do 
	showGraph Samples.sample_helloWorld
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






--insN :: [Node] -> State Graph ()
--insN n = State $ \s -> DG.insNodes n g

--insE :: [Edge] -> State Graph ()
--insE e = State $ \s -> DG.insEdges e g

--graphManip :: State Graph Graph
--graphManip = do
--	insN (zip [1..] [Node "ala", Node "bob"])
--	insE [(1,2,Edge "a" "b" Standard)]


