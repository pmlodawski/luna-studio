module Empire.API.Data.Output where

import           Data.Binary               (Binary)
import           Empire.API.Data.Port      (InPort)
import           Empire.API.Data.ValueType (ValueType)
import qualified Empire.API.Data.ValueType as ValueType
import           Prologue



data Output = Output { _valueType :: ValueType
                     , _inPort :: InPort
                     } deriving (Show, Eq, Generic)

makeLenses ''Output
instance Binary Output
