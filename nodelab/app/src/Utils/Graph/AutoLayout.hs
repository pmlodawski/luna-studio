module Utils.Graph.AutoLayout where

import           Utils.PreludePlus
import           Control.Monad.State
import           Utils.Graph.Graph      (Graph, fromConnections, neighbourhood, getNodes)
import           Utils.Graph.Components (splitComponents)
import           Utils.Graph.Layers     (assignLayers, getTotalHeight)
import qualified Data.Map               as Map
import           Data.Map               (Map)
import           Utils.Vector           (Vector2(..))
import           Debug.Trace
import           Empire.API.Data.Node (NodeId)

autoLayout :: [NodeId] -> [(NodeId, NodeId)] -> Double -> Double -> Map NodeId (Vector2 Double)
autoLayout nodes edges gridWidth gridHeight = Map.map (toPixels gridWidth gridHeight) organizedGraph where
    organizedGraph  = organizeLayers componentLayers
    componentLayers = assignLayers <$> componentGraphs
    componentGraphs = uncurry fromConnections <$> components
    components      = splitComponents nodes edges

toPixels :: Double -> Double -> (Int, Int) -> Vector2 Double
toPixels gridWidth gridHeight (x, y) = Vector2 (fromIntegral x * gridWidth)
                                               (fromIntegral y * gridHeight)

organizeLayers :: [Map NodeId Int] -> Map NodeId (Int, Int)
organizeLayers componentLayers = mergeComponents (traceShow corrected corrected) where
    corrected   = zipWith moveComponent corrections withHeights
    withHeights = assignHeights <$> componentLayers
    corrections = prefixSum $ traceShowId heights
    heights     = getTotalHeight <$> componentLayers

prefixSum :: [Int] -> [Int]
prefixSum = scanl (+) 0

mergeComponents :: [Map NodeId (Int, Int)] -> Map NodeId (Int, Int)
mergeComponents = Map.fromList . concatMap Map.toList

moveComponent :: Int -> Map NodeId (Int, Int) -> Map NodeId (Int, Int)
moveComponent correction layers = Map.map (\(a, b) -> (a, b + correction)) layers

assignHeights :: Map NodeId Int -> Map NodeId (Int, Int)
assignHeights = Map.fromList . concat . fmap addHeights . groupByLayer . Map.toList

groupByLayer :: [(NodeId, Int)] -> [[(NodeId, Int)]]
groupByLayer = groupBy sameLayer . sortBy compareLayers

sameLayer :: (NodeId, Int) -> (NodeId, Int) -> Bool
sameLayer (_, a) (_, b) = a == b

compareLayers :: (NodeId, Int) -> (NodeId, Int) -> Ordering
compareLayers (_, a) (_, b) = compare a b

addHeights :: [(NodeId, Int)] -> [(NodeId, (Int, Int))]
addHeights = zipWith (\y (n, x) -> (n, (x, y))) [0..]
