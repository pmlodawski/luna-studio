module Empire.Objects.DefaultValue where

import Prologue

data Value = IntValue    Int
           | StringValue String
           deriving (Show, Eq)

data PortDefault = Expression String | Constant Value deriving (Show, Eq)
