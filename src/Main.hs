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
import qualified Luna
import qualified Luna.GraphSerialization as GS

noEdges :: [DG.UEdge]
noEdges = [] 

main :: IO ()
main = do
	print $ example_1
	--print $ runState graphManip DG.empty
	return ()


example_1 :: Luna.Graph
example_1 = let
	manager = DefManager.empty
	manager' = DefManager.insert "ala" (Luna.Function $ Luna.NodeDef "f" g3 [] []) manager

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


