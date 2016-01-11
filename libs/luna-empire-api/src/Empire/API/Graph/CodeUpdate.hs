module Empire.API.Graph.CodeUpdate where

import           Prologue
import           Data.Binary             (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node    (Node)
import           Data.Text.Lazy (Text)

data Update = Update { _location  :: GraphLocation
                     , _code      :: Text
                     } deriving (Generic, Show, Eq)

makeLenses ''Update

instance Binary Update
