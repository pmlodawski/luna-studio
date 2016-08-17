module Empire.API.Data.DefaultValue where

import           Data.Binary          (Binary)
import           Data.Text.Lazy       as Text
import           Prologue

import qualified Graphics.API      as G


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
           | Histogram      [(Int, Int)]
           | Image          String Double Double
           | DataFrame      [(String, [Value])]
           | Graphics       G.Graphics
           | Lambda         String
           deriving (Generic, Show, Eq)

data PortDefault = Expression String | Constant Value deriving (Generic, Show, Eq)

instance Binary Value
instance Binary PortDefault

makePrisms ''Value
makePrisms ''PortDefault

stringify :: Value -> Text
stringify = Text.pack . show
