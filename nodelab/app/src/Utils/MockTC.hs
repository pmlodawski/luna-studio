module Utils.MockTC where

import qualified Utils.MockHelper as MockHelper
import           Utils.MockHelper (NodeType(NodeType))
import           Utils.PreludePlus

import qualified Object.Port      as Port
import           Object.Port      (Port(Port))
import qualified Object.Node      as Node
import           Object.Node      (Node(Node))




connect :: Port -> Port -> Node -> Maybe Node
connect portFrom portTo node = Node.nodeType (MockHelper.applyType pidTo typeFrom) node
    where typeFrom = portFrom ^. Port.portValueType
          pidTo    = portTo   ^. Port.portId


disconnect :: Port -> Port -> Node -> Maybe Node
disconnect portFrom portTo node = Node.nodeType (MockHelper.unapplyType pidTo typeFrom) node
    where typeFrom = portFrom ^. Port.portValueType
          pidTo    = portTo   ^. Port.portId
