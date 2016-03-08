module Empire.API.Control.EmpireStarted where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node)

data Status = Status { } deriving (Generic, Show, Eq)

makeLenses ''Status

instance Binary Status
