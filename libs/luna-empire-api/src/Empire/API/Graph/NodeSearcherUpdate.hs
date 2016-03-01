module Empire.API.Graph.NodeSearcherUpdate where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.NodeSearcher  (LunaModule)

data Update = Update { _location         :: GraphLocation
                     , _nodeSearcherData :: LunaModule
                     } deriving (Generic, Show, Eq)

makeLenses ''Update

instance Binary Update
