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
import qualified Luna.Library as Library
import qualified System.UniPath as Path

import qualified Luna.Graph as Graph
import qualified Luna.Samples as Samples
import qualified Data.GraphViz as GV


--import Text.Show.Pretty
--import Text.Groom

defaultVis :: (DG.Graph gr) => gr nl el -> GV.DotGraph DG.Node
defaultVis = GV.graphToDot GV.nonClusteredParams

main :: IO ()
main = do 
	let
		lib = Library.Library $ Path.fromUnixString "/home/wdanilo/tmp/lunalib/std.node"
		manager = DefManager.load lib
			    $ DefManager.empty
	print manager
	-- showGraph Samples.sample_helloWorld
	return ()

--showGraph :: (Graph.Graph, DefManager.DefManager) -> IO()
--showGraph (graph, manager) = do 
--	print graph
--	print manager
--	print $ defaultVis $ Graph.repr graph -- prints dot graphviz representation
--	GV.preview $ Graph.repr graph -- shows interactive view while compiling from sublime
--	--return ()

