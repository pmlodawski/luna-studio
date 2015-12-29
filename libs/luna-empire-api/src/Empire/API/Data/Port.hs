module Empire.API.Data.Port where

import Prologue
import Data.Binary

import Empire.API.Data.DefaultValue (PortDefault)

data InPort  = Self | Arg Int        deriving (Generic, Show, Eq)
data OutPort = All  | Projection Int deriving (Generic, Show, Eq)

instance Binary InPort
instance Binary OutPort

data PortId = InPortId InPort | OutPortId OutPort deriving (Show, Eq)

type ValueType = ()

data Port = Port { portId       :: PortId
                 , valueType    :: ValueType
                 , defaultValue :: Maybe PortDefault
                 } deriving (Show, Eq)

makeLenses ''Port
