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
            deriving (Eq, Show)

portIdToNum :: PortId -> Int
portIdToNum AllPorts    = 0
portIdToNum (PortNum n) = n

type PortIdCollection = [PortId]

data PortType = InputPort | OutputPort deriving (Eq, Show)

instance PrettyPrinter PortId where
    display portId = "pId(" <> show portId <> ")"
