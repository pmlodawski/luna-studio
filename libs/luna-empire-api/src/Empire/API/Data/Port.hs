module Empire.API.Data.Port where

import Prologue
import Data.Binary                  (Binary)

import Empire.API.Data.DefaultValue (PortDefault)
import Empire.API.Data.ValueType    (ValueType)

data InPort  = Self | Arg Int        deriving (Generic, Show, Eq)
data OutPort = All  | Projection Int deriving (Generic, Show, Eq)

instance Binary InPort
instance Binary OutPort

data PortId = InPortId InPort | OutPortId OutPort deriving (Generic, Show, Eq)

instance Ord PortId where
  (InPortId  _) `compare` (OutPortId _) = LT
  (OutPortId _) `compare` (InPortId  _) = GT
  (InPortId  a) `compare` (InPortId  b) = a `compare` b
  (OutPortId a) `compare` (OutPortId b) = a `compare` b

instance Ord InPort where
  Self `compare` Self = EQ
  Self `compare` (Arg _) = LT
  (Arg _) `compare` Self = GT
  (Arg a) `compare` (Arg b) = a `compare` b

instance Ord OutPort where
  All            `compare` All            = EQ
  All            `compare` (Projection _) = LT
  (Projection _) `compare` All            = GT
  (Projection a) `compare` (Projection b) = a `compare` b

instance Read InPort where
    readsPrec _ ('S':'e':'l':'f':rest) = [(Self, rest)]
    readsPrec _ ('s':'e':'l':'f':rest) = [(Self, rest)]
    readsPrec d r = do
        (v, r') <- readsPrec d r
        return (Arg v, r')

instance Read OutPort where
    readsPrec _ ('A':'l':'l':rest) = [(All, rest)]
    readsPrec _ ('a':'l':'l':rest) = [(All, rest)]
    readsPrec d r = do
        (v, r') <- readsPrec d r
        return (Projection v, r')

data PortState = NotConnected | Connected | WithDefault PortDefault deriving (Show, Eq, Generic)

data Port = Port { _portId     :: PortId
                 , _name       :: String
                 , _valueType  :: ValueType
                 , _state      :: PortState
                 } deriving (Show, Eq, Generic)

makeLenses ''Port
instance Binary PortId
instance Binary Port
instance Binary PortState


