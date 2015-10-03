module Object.Object where

import           Utils.PreludePlus

newtype Object a = Object { fromObject :: a } deriving (Eq, Show)

makeLenses ''Object

instance Unwrap Object where unwrap = fromObject

instance PrettyPrinter a => PrettyPrinter (Object a) where
    display (Object o) = "o(" <> display o <> ")"

type ID = Int


type NodeId = ID
type ConnectionId = ID

type NodeIdCollection = [NodeId]

data PortId = AllPorts
            | PortNum ID
            deriving (Ord, Eq, Show)

--instance Ord PortId where
--    AllPorts    `compare` AllPorts    = EQ
--    AllPorts    `compare` _           = LT
--    _           `compare` AllPorts    = GT
--    (PortNum a) `compare` (PortNum b) = a `compare` b

portIdToNum :: PortId -> Int
portIdToNum AllPorts    = 0
portIdToNum (PortNum n) = n

createInputPortId :: Int -> PortId
createInputPortId num = PortNum num

createOutputPortId :: Int -> PortId
createOutputPortId 0   = AllPorts
createOutputPortId num = PortNum (num - 1)


type PortIdCollection = [PortId]

data PortType = InputPort | OutputPort deriving (Ord, Eq, Show)

instance PrettyPrinter PortId where
    display portId = "pId(" <> show portId <> ")"
