module Empire.Objects.Port where

import Prologue
import Empire.Objects.DefaultValue (PortDefault)

data InPort  = Self | Arg Int        deriving (Show, Eq)
data OutPort = All  | Projection Int deriving (Show, Eq)

data PortId = InPortId InPort | OutPortId OutPort deriving (Show, Eq)

type ValueType = ()

data Port = Port { portId       :: PortId
                 , valueType    :: ValueType
                 , defaultValue :: Maybe PortDefault
                 } deriving (Show, Eq)

makeLenses ''Port
