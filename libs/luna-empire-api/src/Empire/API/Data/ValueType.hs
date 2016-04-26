{-# LANGUAGE DeriveGeneric #-}

module Empire.API.Data.ValueType where

import           Data.Binary (Binary)
import           Prologue
import           Data.Hashable (Hashable)
import           Empire.API.Data.TypeRep (TypeRep (..))

data ValueType = AnyType | TypeIdent TypeRep deriving (Show, Eq, Generic)

data ValueTypeEnum = DiscreteNumber
                   | ContinuousNumber
                   | String
                   | Bool
                   | Other
                   deriving (Show, Eq, Enum, Generic)

instance Binary ValueType
instance Binary ValueTypeEnum
makeLenses ''ValueType

toEnum' :: ValueType -> ValueTypeEnum
toEnum' (TypeIdent (TCons name _)) = case name of
  "Int"    -> DiscreteNumber
  "Long"   -> DiscreteNumber
  "Float"  -> ContinuousNumber
  "Double" -> ContinuousNumber
  "String" -> String
  "Bool"   -> Bool
  _        -> Other
toEnum' _ = Other

toEnum :: Getter ValueType ValueTypeEnum
toEnum = to toEnum'
