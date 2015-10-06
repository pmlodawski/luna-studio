module Utils.Graph.Graph ( Graph(..)
                         , fromEdgesList
                         , neighbourhood
                         , transposeGraph
                         ) where

import           Utils.PreludePlus
import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Tuple          (swap)

data Graph a = Graph { _neighbourhood :: Map a [a] } deriving (Show, Eq)
makeLenses ''Graph

fromEdgesList :: Ord a => [a] -> [(a, a)] -> Graph a
fromEdgesList nodes edges = Graph $ execState (addEdges edges) (initialMap nodes)

toEdgesList :: Graph a -> ([a], [(a, a)])
toEdgesList (Graph m) = (nodes, edges) where
    nodes = Map.keys m
    edges = concatMap (uncurry nodeToEdges) $ Map.toList m

nodeToEdges :: a -> [a] -> [(a, a)]
nodeToEdges node neighs = zip (repeat node) neighs

transposeGraph :: Ord a => Graph a -> Graph a
transposeGraph graph = fromEdgesList nodes (swap <$> edges) where
    (nodes, edges) = toEdgesList graph

initialMap :: Ord a => [a] -> Map a [a]
initialMap nodes = fromList $ zip nodes (repeat [])

addEdges :: Ord a => [(a, a)] -> State (Map a [a]) ()
addEdges edges = mapM_ (uncurry addEdge) edges

addEdge :: Ord a => a -> a -> State (Map a [a]) ()
addEdge src dst = modify $ at src . non [] %~ (dst :)
