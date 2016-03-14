module Empire.API.Data.DefaultValue where

import Prologue
import Data.Binary   (Binary)
import Data.Text.Lazy as Text

data Value = IntValue       Int
           | DoubleValue    Double
           | RationalValue  Rational
           | BoolValue      Bool
           | StringValue    String
           | IntList        [Int]
           | DoubleList     [Double]
           | BoolList       [Bool]
           | StringList     [String]
           | DoublePairList [(Double, Double)]
           | IntPairList    [(Int, Int)]
           | Histogram      [(Double, Double)]
           | Image          String Double Double
           | DataFrame      [(String, [Value])]
           deriving (Generic, Show, Eq)

data PortDefault = Expression String | Constant Value deriving (Generic, Show, Eq)

instance Binary Value
instance Binary PortDefault

makePrisms ''Value
makePrisms ''PortDefault

stringify :: Value -> Text
stringify = Text.pack . show
