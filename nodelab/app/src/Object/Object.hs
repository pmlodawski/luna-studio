module Object.Object where

import Utils.PreludePlus
import Data.Aeson (ToJSON)

type ID = Int

type NodeId = ID
type ConnectionId = ID

type NodeIdCollection = [NodeId]

data PortId = AllPorts
            | PortNum ID
            deriving (Ord, Eq, Show, Generic)

instance ToJSON PortId

portIdToNum :: PortId -> Int
portIdToNum AllPorts    = 0
portIdToNum (PortNum n) = n

createInputPortId :: Int -> PortId
createInputPortId num = PortNum num

createOutputPortId :: Int -> PortId
createOutputPortId 0   = AllPorts
createOutputPortId num = PortNum (num - 1)


type PortIdCollection = [PortId]

data PortType = InputPort | OutputPort deriving (Ord, Eq, Show, Generic)

instance ToJSON PortType
