module Utils.Graph.Graph ( Graph(..)
                         , fromConnections
                         , neighbourhood
                         , transposeGraph
                         , getNodes
                         , getEdges
                         ) where

import           Utils.PreludePlus
import           Control.Monad.State
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Tuple          (swap)

data Graph a = Graph { _neighbourhood :: Map a [a] } deriving (Show, Eq)
makeLenses ''Graph

fromConnections :: Ord a => [a] -> [(a, a)] -> Graph a
fromConnections nodes edges = Graph $ execState (addEdges edges) (initialMap nodes)

toConnections :: Graph a -> ([a], [(a, a)])
toConnections (Graph m) = (nodes, edges) where
    nodes = Map.keys m
    edges = concatMap (uncurry nodeToEdges) $ Map.toList m

nodeToEdges :: a -> [a] -> [(a, a)]
nodeToEdges node neighs = zip (repeat node) neighs

transposeGraph :: Ord a => Graph a -> Graph a
transposeGraph graph = fromConnections nodes (swap <$> edges) where
    (nodes, edges) = toConnections graph

initialMap :: Ord a => [a] -> Map a [a]
initialMap nodes = fromList $ zip nodes (repeat [])

getNodes :: Ord a => Graph a -> [a]
getNodes = fst . toConnections

getEdges :: Ord a => Graph a -> [(a, a)]
getEdges = snd . toConnections

addEdges :: Ord a => [(a, a)] -> State (Map a [a]) ()
addEdges edges = mapM_ (uncurry addEdge) edges

addEdge :: Ord a => a -> a -> State (Map a [a]) ()
addEdge src dst = modify $ at src . non [] %~ (dst :)
