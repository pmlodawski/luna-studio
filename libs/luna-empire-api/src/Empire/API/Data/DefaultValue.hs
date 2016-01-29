module Empire.API.Data.DefaultValue where

import Prologue
import Data.Binary   (Binary)

data Value = IntValue    Int
           | DoubleValue Double
           | BoolValue   Bool
           | StringValue String
           | IntList     [Int]
           | DoubleList  [Double]
           | BoolList    [Bool]
           | StringList  [String]
           deriving (Generic, Show, Eq)

data PortDefault = Expression String | Constant Value deriving (Generic, Show, Eq)

instance Binary Value
instance Binary PortDefault

makePrisms ''Value
makePrisms ''PortDefault
