module Empire.API.Data.Input where

import           Data.Binary               (Binary)
import           Empire.API.Data.ValueType (ValueType)
import           Prologue


data Input = Input { _name :: Text
                   , _valueType :: ValueType
                   } deriving (Show, Eq, Generic)

makeLenses ''Input
instance Binary Input
