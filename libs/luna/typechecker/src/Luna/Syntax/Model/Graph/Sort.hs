{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Luna.Syntax.Model.Graph.Sort where

import           Data.Container.Class
import qualified Data.List            as List
import           Prologue



class (Graph g, BackEdgedGraph g, MarkableGraph g, Eq (Item g)) => CompleteGraph g

class Graph g where
    listNodes :: g -> [Item g]

class BackEdgedGraph g where
    predecessors :: Item g -> g -> [Item g]

class MarkableGraph g where
    markNode :: Item g -> g -> g
    isMarked :: Item g -> g -> Bool

class SortBy g where
    sortBy :: (Item g -> Bool) -> g -> Either SortError [Item g]

data SortError = GraphContainsCycle

instance CompleteGraph g => SortBy g where
    sortBy predicate g' = sortBy' [] (listNodes g') g' where
        sortBy' queue1 queue2 g = case findReady queue1 g of
            (Just node, rest) -> (node :) <$> sortBy' rest queue2 (markNode node g)
            (Nothing  , _   ) -> case findReady queue2 g of
                (Just node, rest) -> (node :) <$> sortBy' queue1 rest (markNode node g)
                (Nothing  , _   ) -> if null queue2
                    then Left GraphContainsCycle
                    else sortBy' (queue1 ++ [head queue2]) (tail queue2) g
        findReady queue g = findExclude (indegIs0 g) queue
        indegIs0 g node = null $ filter (predicate' g) $ predecessors node g
        predicate' g node = predicate node && isMarked node g


-- TODO[PM] : move it to list utils
findExclude :: (a -> Bool) -> [a] -> (Maybe a, [a])
findExclude predicate (h:t) = if predicate h
    then (Just h, t)
    else (found, h:rest) where
        (found, rest) = findExclude predicate t
