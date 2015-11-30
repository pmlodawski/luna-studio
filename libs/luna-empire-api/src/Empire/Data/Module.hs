module Empire.Data.Module where

import Prologue
import Empire.Data.Function (Function)

data Module = Module { _name    :: String
                     , _methods :: [Function]
                     } deriving (Show)

makeLenses ''Module
