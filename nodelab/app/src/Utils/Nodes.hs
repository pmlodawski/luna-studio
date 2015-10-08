module Utils.Nodes where

import           Utils.PreludePlus
import qualified Utils.MockHelper  as MockHelper
import           Utils.Vector      (Vector2)

import qualified Object.Node       as Node
import           Object.Node       (Node(Node))
import qualified Object.Port       as Port
import           Object.Port       (Port(Port))
import qualified Object.Object     as Object

import qualified Data.IntMap.Lazy  as IntMap


getNodeOutType :: Node -> Port.ValueType
getNodeOutType = head . MockHelper.getOutputType . view Node.nodeType


getNodePos :: Node.NodesMap -> Object.NodeId -> Vector2 Double
getNodePos nodesMap nodeId = node ^. Node.nodePos where
    node = IntMap.findWithDefault (error $ "Node " <> show nodeId <> " not found") nodeId nodesMap


getPortByRef :: Node.PortRef -> Node.NodesMap -> Maybe Port
getPortByRef portRef nodesMap = port where
    nodeId = portRef ^. Node.refPortNodeId
    node   = IntMap.findWithDefault (error $ "Node " <> show nodeId <> " not found") nodeId nodesMap
    ports' = case portRef ^. Node.refPortType of
        Object.InputPort  -> node ^. Node.ports . Node.inputPorts
        Object.OutputPort -> node ^. Node.ports . Node.outputPorts
    port   = find (\p -> p ^. Port.portId == portRef ^. Node.refPortId) ports'


getNodeByRef :: Node.PortRef -> Node.NodesMap -> Maybe Node
getNodeByRef portRef nodesMap = node where
    nodeId  = portRef ^. Node.refPortNodeId
    nodeErr = error $ "Node " <> show nodeId <> " not found"
    node    = IntMap.lookup nodeId nodesMap


getPortAngle :: Node.PortRef -> Node.NodesMap -> Double
getPortAngle portRef nodesMap = case getPortByRef portRef nodesMap of
    Just port -> port ^. Port.angle
    Nothing   -> case portRef ^. Node.refPortType of
        Object.InputPort  -> 1.3 * pi
        Object.OutputPort -> 1.7 * pi


tryGetSrcDst :: Node.PortRef -> Node.PortRef -> Maybe (Node.PortRef, Node.PortRef)
tryGetSrcDst portRef1 portRef2 = case (portRef1 ^. Node.refPortType, portRef2 ^. Node.refPortType) of
    (Object.OutputPort, Object.InputPort)  -> Just (portRef1, portRef2)
    (Object.InputPort,  Object.OutputPort) -> Just (portRef2, portRef1)
    _                        -> Nothing

