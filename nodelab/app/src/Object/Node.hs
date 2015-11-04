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

import           Object.Dynamic
import           Object.Object
import           Object.Port


import qualified Data.Text.Lazy   as Text
import           Data.Text.Lazy   (Text)
import qualified Data.IntMap.Lazy as IntMap
import           Data.IntMap.Lazy (IntMap)

import           Reactive.State.Camera (Camera, screenToWorkspace)

import System.IO.Unsafe (unsafePerformIO)


data Ports = Ports { _inputPorts  :: PortCollection
                   , _outputPorts :: PortCollection
                   } deriving (Eq, Show)

data Node = Node { _nodeId      :: NodeId
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
    def = Node (-1) def "" def (NodeType 0 [])

instance PrettyPrinter Ports where
    display (Ports input output)
        = display input <> " " <> display output

instance PrettyPrinter Node where
    display (Node ident pos expr ports _)
        = "n(" <> display ident
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
createPort portType valueType allPorts ident = Port ident valueType


createPorts' :: [ValueType] -> [ValueType] -> Ports
createPorts' inputPortTypes outputPortTypes = Ports inputPorts outputPorts where
    inputPortsNum  = length inputPortTypes
    outputPortsNum = length outputPortTypes
    inputPorts     = (\num -> Port (createInputPortId  num) (inputPortTypes  !! num)) <$> take inputPortsNum  nat
    outputPorts    = (\num -> Port (createOutputPortId num) (outputPortTypes !! num)) <$> take outputPortsNum nat
    nat            = [0..]


createPorts :: Text -> Ports
createPorts expr = createPorts' inputs outputs
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
isModule node = fromMaybe False $ isUpper <$> firstLetter where
    firstLetter = maybeHead $ Text.unpack $ node ^. expression
    maybeHead = listToMaybe
