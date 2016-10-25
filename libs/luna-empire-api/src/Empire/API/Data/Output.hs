module Empire.API.Data.Output where

import           Data.Binary               (Binary)
import           Empire.API.Data.ValueType (ValueType)
import           Prologue


data Output = Output { _valueType :: ValueType
                     } deriving (Show, Eq, Generic)

makeLenses ''Output
instance Binary Output
