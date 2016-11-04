module Empire.API.Data.Input where

import           Data.Binary               (Binary)
import           Empire.API.Data.Port      (OutPort)
import           Empire.API.Data.ValueType (ValueType)
import           Prologue



data Input = Input { _name :: Text
                   , _valueType :: ValueType
                   , _outPort :: OutPort
                   } deriving (Show, Eq, Generic)

makeLenses ''Input
instance Binary Input
