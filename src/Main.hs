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
	--showGraph Samples.sample_helloWorld
	return ()

showGraph :: (Graph.Graph, DefManager.DefManager) -> IO()
showGraph (graph, manager) = do 
	print graph
	print manager
	print $ defaultVis graph -- prints dot graphviz representation
	GV.preview graph -- shows interactive view while compiling from sublime
	return ()

