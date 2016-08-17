module Empire.API.Data.Error where

import Prologue
import Data.Binary (Binary)

data Error t = ImportError String | NoMethodError String t | TypeError t t | RuntimeError String deriving (Show, Eq, Generic)

instance Binary t => Binary (Error t)
