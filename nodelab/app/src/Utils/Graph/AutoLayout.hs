module Utils.Graph.AutoLayout where

import           Utils.PreludePlus
import           Utils.Graph.TopSort (topsort)
import           Utils.Graph.Graph   (fromEdgesList)
import           Object.Object       (NodeId)
import qualified Data.Map            as Map
import           Data.Map            (Map)
import           Utils.Vector        (Vector2(..))
import           Debug.Trace

autoLayout :: [NodeId] -> [(NodeId, NodeId)] -> Double -> Double -> Map NodeId (Vector2 Double)
autoLayout nodes edges gridWidth gridHeight = Map.fromList $ zip (trace ("dupa " ++ (show topsorted)) topsorted) coords where
    topsorted = topsort $ fromEdgesList nodes edges
    coords    = zipWith Vector2 ((gridWidth *) <$> xCoords) ((gridHeight *) <$> yCoords)
    xCoords   = [0.0 ..]
    yCoords   = cycle [-1.0, 0.0, 1.0]

assignLayers :: Graph NodeId -> [NodeId] -> Map NodeId Int
assignLayers = undefined
