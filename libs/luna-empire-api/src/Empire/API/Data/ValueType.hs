{-# LANGUAGE DeriveGeneric #-}

module Empire.API.Data.ValueType where

import           Data.Binary (Binary)
import           Prologue
import           Data.Hashable (Hashable)

data ValueType = AnyType | TypeIdent String deriving (Show, Eq, Generic)

data ValueTypeEnum = DiscreteNumber
                   | ContinuousNumber
                   | String
                   | Bool
                   | Other
                   deriving (Show, Eq, Enum, Generic)

instance Binary ValueType
instance Binary ValueTypeEnum
instance Hashable ValueType
makeLenses ''ValueType

toEnum' :: ValueType -> ValueTypeEnum
toEnum' AnyType = Other
toEnum' (TypeIdent name) = case name of
  "Int"    -> DiscreteNumber
  "Long"   -> DiscreteNumber
  "Float"  -> ContinuousNumber
  "Double" -> ContinuousNumber
  "String" -> String
  "Bool"   -> Bool
  _        -> Other

toEnum :: Getter ValueType ValueTypeEnum
toEnum = to toEnum'
