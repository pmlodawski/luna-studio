module Empire.API.Graph.TypeCheck where

import           Prologue
import           Data.Binary                   (Binary)
import           Data.Text.Lazy                (Text)

import           Empire.API.Data.GraphLocation (GraphLocation)

data Request = Request { _location :: GraphLocation
                       } deriving (Generic, Show, Eq)

makeLenses ''Request

instance Binary Request
