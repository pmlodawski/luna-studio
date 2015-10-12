{-# LANGUAGE OverloadedStrings #-}

module Object.Node where

import           Control.Monad (msum)

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.Angle
import           Utils.MockHelper (NodeType(NodeType))
import qualified Utils.MockHelper as MHelper
import           Data.Dynamic
import           Debug.Trace

import           JS.Camera
import           Object.Dynamic
import           Object.Object
import           Object.Port


import qualified Data.Text.Lazy   as Text
import           Data.Text.Lazy   (Text)
import qualified Data.IntMap.Lazy as IntMap
import           Data.IntMap.Lazy (IntMap)

import System.IO.Unsafe (unsafePerformIO)


data Ports = Ports { _inputPorts  :: PortCollection
                   , _outputPorts :: PortCollection
                   } deriving (Eq, Show)

data Node = Node { _nodeId      :: NodeId
                 , _selected    :: Bool
                 , _nodePos     :: Vector2 Double
                 , _expression  :: Text
                 , _ports       :: Ports
                 , _nodeType    :: NodeType
                 } deriving (Eq, Show, Typeable)

makeLenses ''Ports
makeLenses ''Node

type NodeCollection   = [Node]
type NodesMap         = IntMap Node

instance Default Ports where
    def = Ports [] []

instance Default Node where
    def = Node (-1) False def "" def (NodeType 0 [])

instance PrettyPrinter Ports where
    display (Ports input output)
        = display input <> " " <> display output

instance PrettyPrinter Node where
    display (Node ident sel pos expr ports _)
        = "n(" <> display ident
        <> " " <> display sel
        <> " " <> display pos
        <> " " <> display expr
        <> " " <> display ports
        <> ")"

isNode :: Object Dynamic -> Bool
isNode obj = isJust (unpackDynamic obj :: Maybe Node)


data PortRef = PortRef { _refPortNodeId :: NodeId
                       , _refPortType   :: PortType
                       , _refPortId     :: PortId
                       } deriving (Eq, Show)

makeLenses ''PortRef


instance PrettyPrinter PortType where
    display = show

instance PrettyPrinter PortRef where
    display (PortRef portNodeId portType portId)
        = "n(" <> display portNodeId
        <> " " <> display portType
        <> " " <> display portId
        <> ")"

instance Ord PortRef where
    (PortRef anid apt apid) `compare` (PortRef bnid bpt bpid)  = anid `compare` bnid
                                                              <> apt  `compare` bpt
                                                              <> apid `compare` bpid

nodeRadius        = 30.0
portSize          = 3.0
portDistFromRim   = 1.0
distFromPort      = 0.3

radiusSquared = nodeRadius * nodeRadius
radiusShadow  = sqrt $ radiusSquared / 2.0


portWidth         = 4.0
portOuterBorder   = nodeRadius + portDistFromRim + portWidth

portOuterBorderSquared = portOuterBorder * portOuterBorder


haloOuterMargin        = 5.0
nodeHaloInnerRadius    = nodeRadius + portDistFromRim
nodeHaloOuterRadius    = nodeHaloInnerRadius + portWidth + haloOuterMargin
haloInnerRadiusSquared = nodeHaloInnerRadius * nodeHaloInnerRadius
haloOuterRadiusSquared = nodeHaloOuterRadius * nodeHaloOuterRadius

closenestFactor        = 0.25


createPort :: PortType -> ValueType -> Int -> PortId -> Port
createPort portType valueType allPorts ident = Port ident valueType $ portDefaultAngle portType allPorts ident


createPorts' :: [ValueType] -> [ValueType] -> Ports
createPorts' inputPortTypes outputPortTypes = Ports inputPorts outputPorts where
    inputPortsNum  = length inputPortTypes
    outputPortsNum = length outputPortTypes
    inputPorts     = (\num -> createPort InputPort  (inputPortTypes  !! num) inputPortsNum  $ createInputPortId  num) <$> take inputPortsNum  nat
    outputPorts    = (\num -> createPort OutputPort (outputPortTypes !! num) outputPortsNum $ createOutputPortId num) <$> take outputPortsNum nat
    nat            = [0..]


createPorts :: Text -> Ports
createPorts expr = seq (unsafePerformIO $ print outputs)createPorts' inputs outputs
    where nodeType = MHelper.getNodeType   expr
          inputs   = MHelper.getInputTypes nodeType
          outputs  = MHelper.getOutputType nodeType


getPorts :: PortType -> Node -> PortCollection
getPorts  InputPort = (^. ports .  inputPorts)
getPorts OutputPort = (^. ports . outputPorts)

setPorts :: PortType -> Ports -> PortCollection -> Ports
setPorts  InputPort allPorts ports = allPorts &  inputPorts .~ ports
setPorts OutputPort allPorts ports = allPorts & outputPorts .~ ports

isDef :: Node -> Bool
isDef node = exprPrefix == "def " where
    exprPrefix = Text.unpack $ Text.take 4 $ node ^. expression

isModule :: Node -> Bool
isModule node = isUpper firstLetter where
    firstLetter = head $ Text.unpack $ node ^. expression

-- getPort :: PortId -> PortType -> Node -> Maybe Port
-- getPort ident = find (\port -> port ^. portId == ident) .: getPorts

-- fromPortRef :: PortRef -> Maybe Port
-- fromPortRef portRef = getPort (portRef ^. refPortId) (portRef ^. refPortType) (portRef ^. refPortNode)

updatePortInPorts :: PortId -> Angle -> PortCollection -> PortCollection
updatePortInPorts refPortId angle ports = tryUpdatePort refPortId angle <$> ports

tryUpdatePort :: PortId -> Angle -> Port -> Port
tryUpdatePort refPortId newAngle port = if port ^. portId == refPortId then updatedPort
                                                                       else port
                                        where updatedPort = port & angle .~ newAngle

updatePortAngle :: PortRef -> Angle -> Node -> Node
updatePortAngle portRef angle node = newNode where
    newNode      = node & ports .~ newAllPorts
    portType     = portRef ^. refPortType
    newAllPorts  = setPorts portType (node ^. ports) newPorts
    oldPorts     = getPorts portType node
    newPorts     = updatePortInPorts (portRef ^. refPortId) angle oldPorts


updateSourcePort :: PortRef -> Angle -> Node -> Node
updateSourcePort portRef angle node = if node ^. nodeId == portRef ^. refPortNodeId then newNode else node where
    newNode = updatePortAngle portRef angle node

updateSourcePortInNodes :: Angle -> PortRef -> NodesMap -> NodesMap
updateSourcePortInNodes angle portRef nodes = updateSourcePort portRef angle <$> nodes


-- updateNodeSelection :: NodeIdCollection -> Node -> Node
-- updateNodeSelection selNodeIds node = let selection = (node ^. nodeId) `elem` selNodeIds in
--     node & selected .~ selection

-- updateNodesSelection :: NodeIdCollection -> NodeCollection -> NodeCollection
-- updateNodesSelection selNodeIds nodes = fmap (updateNodeSelection selNodeIds) nodes

getNodesAt :: Vector2 Int -> Camera -> NodeCollection -> NodeCollection
getNodesAt posScr camera nodes = filter closeEnough nodes where
    pos              = screenToWorkspace camera posScr
    closeEnough node = inRange && inRadius where
        inRange      = dist ^. x < nodeRadius && dist ^. y < nodeRadius
        inRadius     = distSquared < radiusSquared
        distSquared  = (dist ^. x) ^ 2 + (dist ^. y) ^ 2
        dist         = (node ^. nodePos - pos)


getNodeIdsAt :: Vector2 Int -> Camera -> NodeCollection -> NodeIdCollection
getNodeIdsAt = (fmap (^. nodeId)) .:. getNodesAt

getPortRefs :: Angle -> Node -> [(Angle, PortRef)]
getPortRefs refAngle node = inputs <> outputs where
    inputs    = newAnglePort  InputPort <$> getPorts  InputPort node
    outputs   = newAnglePort OutputPort <$> getPorts OutputPort node
    newAnglePort tpe port = (angleDiff refAngle $ port ^. angle, PortRef (node ^. nodeId) tpe $ port ^. portId)


getNodeHaloAt :: Vector2 Int -> Camera -> NodeCollection -> Maybe Node
getNodeHaloAt posScr camera nodes = listToMaybe $ filter inHalo nodes where
    pos              = screenToWorkspace camera posScr
    inHalo node      = inRange && betweenRadii where
        inRange      = dist ^. x < nodeHaloOuterRadius && dist ^. y < nodeHaloOuterRadius
        betweenRadii = haloInnerRadiusSquared < distSquared && distSquared < haloOuterRadiusSquared
        distSquared  = (dist ^. x) ^ 2 + (dist ^. y) ^ 2
        dist         = (node ^. nodePos - pos)

getPortRef :: Vector2 Int -> Camera -> NodeCollection -> Maybe PortRef
getPortRef posScr camera nodes = maybePortRef where
    pos                 = screenToWorkspace camera posScr
    maybePortRef        = do
        nodeHalo       <- getNodeHaloAt posScr camera nodes
        let relVect     = pos - (nodeHalo ^. nodePos)
            posAngle    = normAngle $ atan2 (relVect ^. y) (relVect ^. x)
            portRefs    = getPortRefs posAngle nodeHalo
        when (null portRefs) Nothing
        let closestPort = minimumBy (compare `on` fst) portRefs
        when (closenestFactor < abs (fst closestPort)) Nothing
        -- trace ("closest " <> display closestPort <> "\nportRefs " <> display portRefs) $
        Just $ snd closestPort


-- TODO: Clever algorithm taking radius into account
getNodeIdsIn :: Vector2 Int -> Vector2 Int -> Camera -> NodeCollection -> NodeIdCollection
getNodeIdsIn (Vector2 x1 y1) (Vector2 x2 y2) camera nodes = (^. nodeId) <$> nodesInBounds where
    leftBottom = screenToWorkspace camera (Vector2 (min x1 x2) (min y1 y2)) - Vector2 radiusShadow radiusShadow
    rightTop   = screenToWorkspace camera (Vector2 (max x1 x2) (max y1 y2)) + Vector2 radiusShadow radiusShadow
    nodesInBounds :: NodeCollection
    nodesInBounds = filter isNodeInBounds nodes
    isNodeInBounds node = let pos = node ^. nodePos in
                          leftBottom ^. x <= pos ^. x && pos ^. x <= rightTop ^. x &&
                          leftBottom ^. y <= pos ^. y && pos ^. y <= rightTop ^. y
