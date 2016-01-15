module Empire.API.Data.ValueType where

import Prologue
import Data.Binary (Binary)

newtype ValueType = ValueType { _unValueType :: String } deriving (Show, Eq, Generic)

data ValueTypeEnum = DiscreteNumber
                   | ContinuousNumber
                   | String
                   | Bool
                   | Other
                   deriving (Show, Eq, Enum, Generic)

instance Binary ValueType
instance Binary ValueTypeEnum

toEnum :: ValueType -> ValueTypeEnum
toEnum (ValueType name) = case name of
  "Int"    -> DiscreteNumber
  "Long"   -> DiscreteNumber
  "Float"  -> ContinuousNumber
  "Double" -> ContinuousNumber
  "String" -> String
  "Bool"   -> Bool
  _        -> Other

