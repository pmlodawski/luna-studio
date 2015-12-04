module Empire.Data.Port where

import Prologue

data InPort = Self | Arg Int deriving (Show, Eq)
data OutPort = All | Projection Int deriving (Show, Eq)
