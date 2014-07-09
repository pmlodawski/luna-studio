---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Data.SetForest where

import Flowbox.Prelude

import           Data.Map (Map)
import qualified Data.Map as Map


data SetTree a = SubForest (SetForest a)
               | Leaf
               deriving (Read, Show, Eq, Ord)

type SetForest a = Map a (SetTree a)



empty :: SetForest a
empty = Map.empty


insert :: Ord a => [a] -> SetForest a -> SetForest a
insert []    forest = forest
insert [a]   forest = if Map.member a forest
                        then forest
                        else Map.insert a Leaf forest
insert (h:t) forest = case Map.lookup h forest of
    Just (SubForest e) -> Map.insert h (SubForest $ insert t e        ) forest
    _                  -> Map.insert h (SubForest $ insert t Map.empty) forest


member :: Ord a => [a] -> SetForest a -> Bool
member []    _      = True
member [a]   forest = Map.member a forest
member (h:t) forest = case Map.lookup h forest of
    Just (SubForest e) -> member t e
    _                  -> False


delete :: Ord a => [a] -> SetForest a -> SetForest a
delete []    _      = Map.empty
delete [a]   forest = Map.delete a forest
delete (h:t) forest = case Map.lookup h forest of
    Just (SubForest e) -> Map.insert h (SubForest $ delete t e) forest
    _                  -> forest


draw :: (Show a) => SetForest a -> String
draw = unlines . map (unlines . drawTree) . Map.toList where

    shift first other = zipWith (++) (first : repeat other)

    drawTree (a, Leaf            ) = [show a]
    drawTree (a, SubForest forest) = show a : drawSubTrees (Map.toList forest)

    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (drawTree t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (drawTree t) ++ drawSubTrees ts
