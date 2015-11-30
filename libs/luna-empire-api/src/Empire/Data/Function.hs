module Empire.Data.Function where

import Prologue
import Empire.Data.AST (AST)


data Function = Function { _name :: String
                         , _ast  :: AST
                         } deriving (Show)
