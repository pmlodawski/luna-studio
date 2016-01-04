module Empire.API.Data.Port where

import Prologue
import Data.Binary                  (Binary)

import Empire.API.Data.DefaultValue (PortDefault)

data InPort  = Self | Arg Int        deriving (Generic, Show, Eq)
data OutPort = All  | Projection Int deriving (Generic, Show, Eq)

instance Binary InPort
instance Binary OutPort

data PortId = InPortId InPort | OutPortId OutPort deriving (Generic, Show, Eq)

instance Binary PortId

type ValueType = ()

data Port = Port { portId       :: PortId
                 , valueType    :: ValueType
                 , defaultValue :: Maybe PortDefault
                 } deriving (Generic, Show, Eq)

makeLenses ''Port

instance Binary Port
