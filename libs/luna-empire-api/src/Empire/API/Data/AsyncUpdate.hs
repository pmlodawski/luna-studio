module Empire.API.Data.AsyncUpdate where

import Prologue
import Empire.API.Data.GraphLocation (GraphLocation)
import Empire.API.Data.Node          (Node)

data AsyncUpdate = NodeUpdate GraphLocation Node deriving (Show, Eq)
