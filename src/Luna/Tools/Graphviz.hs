---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.Tools.Graphviz(
showGraph
) where

import qualified Data.Graph.Inductive as DG
import qualified Data.GraphViz as GV

import           Luna.Edge       (Edge)
import qualified Luna.Graph    as Graph
import           Luna.Graph      (Graph)
import           Luna.Node       (Node)


showGraph :: Graph -> IO ()
showGraph graph = do 
	let 
		graphrepr = Graph.repr graph
	print $ defaultVis graphrepr  -- prints dot graphviz representation
	GV.preview $ graphrepr-- shows interactive view while compiling from sublime

defaultVis :: (DG.Graph gr) => gr nl el -> GV.DotGraph DG.Node
defaultVis = GV.graphToDot GV.nonClusteredParams


instance GV.Labellable Edge where
	toLabelValue = GV.toLabelValue . show

instance GV.Labellable Node where
        toLabelValue = GV.toLabelValue . show
