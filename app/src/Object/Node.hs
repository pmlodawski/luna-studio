module Object.Node where

import           Control.Applicative
import           Control.Lens
import           Data.Dynamic
import           Data.Monoid
import           Data.Maybe

import           JS.Camera
import           Object.Dynamic
import           Object.Object
import           Object.Port
import           Utils.Vector
import           Utils.Wrapper
import           Utils.PrettyPrinter


import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy ( Text )

type NodeId = ID

data Node = Node { _nodeId      :: NodeId
                 , _selected    :: Bool
                 , _position    :: Vector2 Double
                 , _expression  :: Text
                 , _inputPorts  :: [Port]
                 , _outputPorts :: [Port]
                 } deriving (Eq, Show, Typeable)

type NodeCollection   = [Node]
type NodeIdCollection = [NodeId]

makeLenses ''Node

instance PrettyPrinter Node where
    display (Node ident sel pos expr inputPorts outputPorts)
        = "n(" <> display ident
        <> " " <> display sel
        <> " " <> display pos
        <> " " <> display expr
        <> " " <> display inputPorts
        <> " " <> display outputPorts
        <> ")"

instance Selectable Node where
    setSelected n selected = n { _selected = selected }
    isSelected  n          = _selected n


isNode :: Object Dynamic -> Bool
isNode obj = isJust (unpackDynamic obj :: Maybe Node)


data PortType = InputPort | OutputPort deriving (Eq, Show)


data PortRef = PortRef { _portNode   :: Node
                       , _portType   :: PortType
                       , _portRefId  :: PortId
                       } deriving (Eq, Show)

makeLenses ''PortRef


instance PrettyPrinter PortType where
    display = show

instance PrettyPrinter PortRef where
    display (PortRef portNode portType portId)
        = "n(" <> display portNode
        <> " " <> display portType
        <> " " <> display portId
        <> ")"


updateNodeSelection :: NodeIdCollection -> Node -> Node
updateNodeSelection selNodeIds node = let selection = (node ^. nodeId) `elem` selNodeIds in
    node & selected .~ selection

updateNodesSelection :: NodeIdCollection -> NodeCollection -> NodeCollection
updateNodesSelection selNodeIds nodes = fmap (updateNodeSelection selNodeIds) nodes

nodeRadius    = 30.0
radiusSquared = nodeRadius * nodeRadius
radiusShadow  = sqrt $ radiusSquared / 2.0

getNodesAt :: Vector2 Int -> Camera -> NodeCollection -> NodeCollection
getNodesAt posScr camera nodes = filter closeEnough nodes where
    pos              = screenToWorkspace camera posScr
    closeEnough node = inRange && inRadius where
        inRange      = dist ^. x < nodeRadius && dist ^. y < nodeRadius
        inRadius     = distSquared < radiusSquared
        distSquared  = (dist ^. x) ^ 2 + (dist ^. y) ^ 2
        dist         = (node ^. position - pos)

-- move to PreludePlus
(.:.) :: (x -> y) -> (a -> b -> c -> x) -> a -> b -> c -> y
(.:.)  = (.) . (.) . (.)

getNodeIdsAt :: Vector2 Int -> Camera -> NodeCollection -> NodeIdCollection
getNodeIdsAt = (fmap (^. nodeId)) .:. getNodesAt

portSize          = 12.0
portWidth         = portSize * sqrt(3.0) / 2.0
portDistFromRim   = 2.0

nodeHaloInnerRadius    = nodeRadius + portDistFromRim
nodeHaloOuterRadius    = nodeHaloInnerRadius + portWidth
haloInnerRadiusSquared = nodeHaloInnerRadius * nodeHaloInnerRadius
haloOuterRadiusSquared = nodeHaloOuterRadius * nodeHaloOuterRadius




getNodeHaloAt :: Vector2 Int -> Camera -> NodeCollection -> Maybe Node
getNodeHaloAt posScr camera nodes = listToMaybe $ filter inHalo nodes where
    pos              = screenToWorkspace camera posScr
    inHalo node      = inRange && betweenRadii where
        inRange      = dist ^. x < nodeHaloOuterRadius && dist ^. y < nodeHaloOuterRadius
        betweenRadii = haloInnerRadiusSquared < distSquared && distSquared < haloOuterRadiusSquared
        distSquared  = (dist ^. x) ^ 2 + (dist ^. y) ^ 2
        dist         = (node ^. position - pos)

getPortRef :: Vector2 Int -> Camera -> NodeCollection -> Maybe PortRef
getPortRef posScr camera nodes = maybePortRef where
    maybePortRef  = findPort <$> getNodeHaloAt posScr camera nodes
    findPort node = PortRef node InputPort 77



-- TODO: Clever algorithm taking radius into account
getNodeIdsIn :: Vector2 Int -> Vector2 Int -> Camera -> NodeCollection -> NodeIdCollection
getNodeIdsIn (Vector2 x1 y1) (Vector2 x2 y2) camera nodes = (^. nodeId) <$> nodesInBounds where
    leftBottom = screenToWorkspace camera (Vector2 (min x1 x2) (max y1 y2)) - Vector2 radiusShadow radiusShadow
    rightTop   = screenToWorkspace camera (Vector2 (max x1 x2) (min y1 y2)) + Vector2 radiusShadow radiusShadow
    nodesInBounds :: NodeCollection
    nodesInBounds = filter isNodeInBounds nodes
    isNodeInBounds node = let pos = node ^. position in
                          leftBottom ^. x <= pos ^. x && pos ^. x <= rightTop ^. x &&
                          leftBottom ^. y <= pos ^. y && pos ^. y <= rightTop ^. y
