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
import qualified Luna.Edge as Edge
import qualified Luna.Graph as Graph
import qualified Luna
import qualified Luna.GraphSerialization as GS

import qualified Luna.DefManager.DefTree as DefTree
import qualified Luna.NodeDef as NodeDef

import Text.Show.Pretty
import Text.Groom

noEdges :: [DG.UEdge]
noEdges = [] 

main :: IO ()
main = do
	let
		d  = DefTree.insert ["std","math"] (Luna.Package $ NodeDef.NodeDef "std" Graph.empty NodeDef.noPorts NodeDef.noPorts) $ 
			 DefTree.insert ["std"] (Luna.Package $ NodeDef.NodeDef "std" Graph.empty NodeDef.noPorts NodeDef.noPorts) $
			 DefTree.empty
	print d
	return ()


example_1 :: Luna.Graph
example_1 = let
	manager = DefManager.empty

	g :: Luna.Graph
	g = DG.empty
	g2 = DG.insNodes (zip [1..] [Luna.Node "ala", Luna.Node "bob"]) g
	g3 = DG.insEdges [(1,2,Luna.Edge "a" "b" Edge.Standard)] g2
	in g3

--insN :: [Node] -> State Graph ()
--insN n = State $ \s -> DG.insNodes n g

--insE :: [Edge] -> State Graph ()
--insE e = State $ \s -> DG.insEdges e g

--graphManip :: State Graph Graph
--graphManip = do
--	insN (zip [1..] [Node "ala", Node "bob"])
--	insE [(1,2,Edge "a" "b" Standard)]


