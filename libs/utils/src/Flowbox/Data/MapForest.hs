---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Flowbox.Data.MapForest where

import           Data.Map   (Map)
import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

import Flowbox.Prelude hiding (children, lookup, toList)



data Node k v = Node { _value    :: Maybe v
                     , _children :: MapForest k v
                     } deriving (Generic, Show, Read, Eq, Ord)

type Path k = [k]

type MapForest k v = Map k (Node k v)


makeLenses ''Node


empty :: MapForest k v
empty = Map.empty


null :: MapForest k v -> Bool
null = Map.null

-- FIXME [pm]: wd: this implementation looks like an inefficient one
toList :: MapForest k v -> [(Path k, v)]
toList = find (const . const True)


fromList :: Ord k => [(Path k, v)] -> MapForest k v
fromList = foldl (\mf (p, val) -> insert p val mf) Flowbox.Data.MapForest.empty


-- this union should be performed with bigger map as the first argument
-- also, it's not a union in a set-theory meaning, it's kind of a join
union :: (Eq k, Ord k) => MapForest k v -> MapForest k v -> MapForest k v
union a b = foldl (\mf (p, val) -> insert p val mf) a (toList b)


keys :: MapForest k v -> [Path k]
keys = map fst . toList


elems :: MapForest k v -> [v]
elems = map snd . toList


member :: Ord k => [k] -> MapForest k v -> Bool
member = Maybe.isJust .: lookup


lookup :: Ord k => [k] -> MapForest k v -> Maybe v
lookup k forest = subPathNode k forest >>= view value

subVals :: Ord k => k -> MapForest k v -> [v]
subVals k forest = case fmap elems (focus k forest) of
    Just els -> els
    Nothing  -> []

subElems :: Ord k => k -> MapForest k v -> [(Path k,v)]
subElems k forest = case fmap toList (focus k forest) of
    Just els -> els
    Nothing  -> []

-- | Insert value v under path [k]
insert :: Ord k => [k] -> v -> MapForest k v -> MapForest k v
insert k = insert' k . Just


delete :: Ord k => [k] -> MapForest k v -> MapForest k v
delete k forest = fixForest $ insert' k Nothing forest


-- | Find entries that match predicate
find :: ([k] -> v -> Bool) -> MapForest k v -> [(Path k, v)]
find predicate = concatMap (find' []) . Map.toList where
    find' key (k, level) = case level ^. value of
        Just v -> if predicate newKey v
            then (newKey, v) : others
            else others
        Nothing -> others
        where
            newKey = key ++ [k]
            others = concatMap (find' newKey) $ Map.toList $ level ^. children

alter :: Ord k => (Maybe v -> Maybe v) -> [k] -> MapForest k v -> MapForest k v
alter fun k forest = insert' k (fun $ lookup k forest ) forest

--------------------------------------------------------------------
-- MapForest internals
--------------------------------------------------------------------

insert' :: Ord k => [k] -> Maybe v -> MapForest k v -> MapForest k v
insert' []    _ forest = forest
insert' [k]   v forest = case Map.lookup k forest of
    Just level -> Map.insert k (level & value .~ v) forest
    _          -> Map.insert k (Node v Map.empty ) forest
insert' (h:t) v forest = case Map.lookup h forest of
    Just level -> Map.insert h (level & children %~ insert' t v     ) forest
    Nothing    -> Map.insert h (Node Nothing $ insert' t v Map.empty) forest


subPathNode :: Ord k => [k] -> MapForest k v -> Maybe (Node k v)
subPathNode []    _      = Nothing
subPathNode [k]   forest = Map.lookup k forest
subPathNode (h:t) forest = case Map.lookup h forest of
    Just level -> subPathNode t $ level ^. children
    _          -> Nothing


subNode :: Ord k => k -> MapForest k v -> Maybe (Node k v)
subNode k forest = Map.lookup k forest

subForest :: Ord k => k -> MapForest k v -> Maybe (MapForest k v)
subForest k forest = fmap (view children) $ subNode k forest


focus :: Ord k => k -> MapForest k v -> Maybe (MapForest k v)
focus k forest = fmap (Map.singleton k) $ subNode k forest

-- FIXME [pm]: co to w ogole za nazwa? ...
fixEntry :: Ord k => (k, Node k v) -> Maybe (k, Node k v)
fixEntry (k, level) = let
    newForest = fixForest $ level ^. children
    newNode  = Just (k, level & children .~ newForest)
    in if Map.null newForest
        then (level ^. value) >> newNode
        else newNode


-- FIXME [pm]: co to w ogole za nazwa? ...
fixForest :: Ord k => MapForest k v -> MapForest k v
fixForest = Map.fromList
          . Maybe.mapMaybe fixEntry
          . Map.toList


draw :: (Show k, Show v) => MapForest k v -> String
draw = unlines . map (unlines . drawTree) . Map.toList where
    shift first other = zipWith (++) (first : repeat other)

    drawTree (k, level) = show (k, level ^. value) : drawSubTrees (Map.toList $ level ^. children)

    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (drawTree t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (drawTree t) ++ drawSubTrees ts


drawKeys :: Show k => MapForest k v -> String
drawKeys = unlines . map (unlines . drawTree) . Map.toList where
    shift first other = zipWith (++) (first : repeat other)

    drawTree (k, level) = show k : drawSubTrees (Map.toList $ level ^. children)

    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (drawTree t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (drawTree t) ++ drawSubTrees ts
