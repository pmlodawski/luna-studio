module Utils.MockTC where

import qualified Utils.MockHelper as MockHelper
import           Utils.MockHelper (NodeType(NodeType))
import qualified Utils.Nodes      as NodeUtils
import           Utils.PreludePlus

import qualified Object.Port      as Port
import           Object.Port      (Port(Port))
import qualified Object.Node      as Node
import           Object.Node      (Node(Node))
import qualified Object.Object    as Object


tryConnect :: Port -> Node -> Node -> Maybe Node
tryConnect portTo nodeFrom nodeTo = Node.nodeType (MockHelper.applyType pidTo typeFrom) nodeTo
    where typeFrom = NodeUtils.getNodeOutType nodeFrom
          pidTo    = portTo   ^. Port.portId


revertNode :: Node -> Node
revertNode node = node & Node.nodeType .~ newTp
    where newTp = MockHelper.getNodeType (node ^. Node.expression)


typecheck :: Node.PortRef -> Node.PortRef -> Node.NodesMap -> Maybe Node
typecheck src dst nodesMap = tryConnect <$> portTo <*> nodeFrom <*> nodeTo & join
    where nodeFrom = NodeUtils.getNodeByRef src nodesMap
          portTo   = NodeUtils.getPortByRef dst nodesMap
          nodeTo   = NodeUtils.getNodeByRef dst nodesMap
