module Empire.API.Control.EmpireStarted where

import           Prologue
import           Data.Aeson                    (ToJSON)
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (Node)
import qualified Empire.API.Topic              as T


data Status = Status { } deriving (Generic, Show, Eq)

makeLenses ''Status

instance Binary Status
instance ToJSON Status

instance T.MessageTopic Status  where topic _ = "empire.control.started.status"
