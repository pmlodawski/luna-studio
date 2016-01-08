module Utils.Graph.Layers where

import           Utils.PreludePlus
import           Control.Monad.State

import qualified Data.Map            as Map
import           Data.Map            (Map)
import           Utils.Graph.Graph   (Graph, neighbourhood, getNodes)
import           Utils.Graph.TopSort (topsort)
import           Empire.API.Data.Node (NodeId)

initLayers :: Graph NodeId -> Map NodeId Int
initLayers = Map.fromList . flip zip [0, 0..] . getNodes

assignLayers :: Graph NodeId -> Map NodeId Int
assignLayers graph = flip execState (initLayers graph) $ mapM_ (layersFromNode graph) topsorted where
    topsorted = topsort graph

getTotalHeight :: Map NodeId Int -> Int
getTotalHeight = maximum . (fmap length) . group . sort . Map.elems

layersFromNode :: Graph NodeId -> NodeId -> State (Map NodeId Int) ()
layersFromNode graph node = do
    currentLayer <- gets $ Map.findWithDefault 0 node
    let neighbours = Map.findWithDefault [] node $ graph ^. neighbourhood
    incLayer currentLayer neighbours

incLayer :: Int -> [NodeId] -> State (Map NodeId Int) ()
incLayer layer neighs = flip mapM_ neighs $ \neigh -> modify $ Map.adjust (max (layer + 1)) neigh
