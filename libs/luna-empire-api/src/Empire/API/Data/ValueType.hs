{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Empire.API.Data.ValueType where

import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Binary             (Binary)
import           Data.Hashable           (Hashable)
import qualified Data.Text               as Text
import           Prologue                hiding (TypeRep)

import           Empire.API.Data.TypeRep (TypeRep (..))



data ValueType = AnyType | TypeIdent TypeRep deriving (Show, Eq, Generic)

data ValueTypeEnum = DiscreteNumber
                   | ContinuousNumber
                   | String
                   | Bool
                   | Other
                   deriving (Show, Eq, Enum, Generic)

makeLenses ''ValueType

instance Binary ValueType
instance Binary ValueTypeEnum

instance ToJSON ValueType
instance FromJSON ValueType
instance ToJSON ValueTypeEnum

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

toText :: Getter ValueType Text
toText = to $ \v -> case v of
    AnyType     -> "*"
    TypeIdent a -> convert a
