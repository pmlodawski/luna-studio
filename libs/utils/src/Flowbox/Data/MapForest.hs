---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Data.MapForest where

import           Data.Map   (Map)
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

import Flowbox.Prelude hiding (Level, lookup)



data Level k v = Level { _value     :: Maybe v
                       , _subForest :: MapForest k v
                       } deriving (Show, Read, Eq, Ord)

type MapForest k v = Map k (Level k v)


makeLenses(''Level)


empty :: MapForest k v
empty = Map.empty


insert :: Ord k => [k] -> v -> MapForest k v -> MapForest k v
insert k v forest = insert' k (Just v) forest


toList :: MapForest k v -> [([k], v)]
toList = find (const . const True)


find :: ([k] -> v -> Bool) -> MapForest k v -> [([k], v)]
find predicate = concatMap (find' []) . Map.toList where
    find' key (k, level) = case level ^. value of
        Just v -> if predicate newKey v
            then (newKey, v) : others
            else others
        Nothing -> others
        where
            newKey = key ++ [k]
            others = concatMap (find' newKey) $ Map.toList $ level ^. subForest


insert' :: Ord k => [k] -> Maybe v -> MapForest k v -> MapForest k v
insert' []    _ forest = forest
insert' [k]   v forest = case Map.lookup k forest of
    Just level -> Map.insert k (level & value .~ v) forest
    _          -> Map.insert k (Level v Map.empty ) forest
insert' (h:t) v forest = case Map.lookup h forest of
    Just level -> Map.insert h (level & subForest %~ insert' t v     ) forest
    Nothing    -> Map.insert h (Level Nothing $ insert' t v Map.empty) forest


lookup :: Ord k => [k] -> MapForest k v -> Maybe v
lookup k forest = sub k forest >>= view value


contains :: Ord k => [k] -> MapForest k v -> Bool
contains = Maybe.isJust .: lookup


sub :: Ord k => [k] -> MapForest k v -> Maybe (Level k v)
sub []    _      = Nothing
sub [k]   forest = Map.lookup k forest
sub (h:t) forest = case Map.lookup h forest of
    Just level -> sub t $ level ^. subForest
    _          -> Nothing


hasChildren :: Ord k => [k] -> MapForest k v -> Bool
hasChildren [] forest = not $ Map.null forest
hasChildren k  forest = Maybe.isJust $ sub k forest


fixEntry :: Ord k => (k, Level k v) -> Maybe (k, Level k v)
fixEntry (k, level) = let
    newForest = fixForest $ level ^. subForest
    newLevel  = Just (k, level & subForest .~ newForest)
    in if Map.null newForest
        then (level ^. value) >> newLevel
        else newLevel


fixForest :: Ord k => MapForest k v -> MapForest k v
fixForest = Map.fromList
          . Maybe.catMaybes
          . map fixEntry
          . Map.toList


delete :: Ord k => [k] -> MapForest k v -> MapForest k v
delete k forest = fixForest $ insert' k Nothing forest


draw :: (Show k, Show v) => MapForest k v -> String
draw = unlines . map (unlines . drawTree) . Map.toList where

    shift first other = zipWith (++) (first : repeat other)

    drawTree (k, level) = show (k, level ^. value) : drawSubTrees (Map.toList $ level ^. subForest)

    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (drawTree t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (drawTree t) ++ drawSubTrees ts
