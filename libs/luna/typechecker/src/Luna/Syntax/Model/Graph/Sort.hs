{-# LANGUAGE UndecidableInstances #-}
module Luna.Syntax.Model.Graph.Sort where

import Prologue
import qualified Data.List as List
import Data.Container.Class

import Luna.Syntax.Model.Graph (Graph)



class SortBy g where
    sortBy :: (Item g -> Bool) -> [Item g] -> g -> [Item g]

instance (Eq (Item g), Predecessors g, MarkNode g) => SortBy g where
    sortBy predicate = sortBy' [] where
        sortBy' queue1 queue2 g = case findReady queue1 g of
            (Just node, rest) -> node : sortBy' rest queue2 (markNode node g)
            (Nothing  , _   ) -> case findReady queue2 g of
                (Just node, rest) -> node : sortBy' queue1 rest (markNode node g)
                (Nothing  , _   ) -> if null queue2
                    then error "Graph contains cycle"
                    else sortBy' (queue1 ++ [head queue2]) (tail queue2) g
        findReady queue g = (found, rest) where
            found = List.find (indegIs0 g) queue
            rest  = case found of
                Just f -> List.delete f queue
                Nothing -> queue
        indegIs0 g node = null $ filter (predicate' g) $ predecessors node g
        predicate' g node = predicate node && isMarked node g

class Predecessors g where
    predecessors :: Item g -> g -> [Item g]

class MarkNode g where
    markNode :: Item g -> g -> g
    isMarked :: Item g -> g -> Bool
