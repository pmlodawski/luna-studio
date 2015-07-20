module Object.Object where

import           Utils.PreludePlus

newtype Object a = Object { fromObject :: a } deriving (Eq, Show)

makeLenses ''Object

instance Unwrap Object where unwrap = fromObject

instance PrettyPrinter a => PrettyPrinter (Object a) where
    display (Object o) = "o(" <> display o <> ")"

type ID = Int


type NodeId = ID
type NodeIdCollection = [NodeId]

type PortId = ID
type PortIdCollection = [PortId]

data PortType = InputPort | OutputPort deriving (Eq, Show)
