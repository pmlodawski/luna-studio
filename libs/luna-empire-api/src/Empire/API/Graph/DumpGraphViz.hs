module Empire.API.Graph.DumpGraphViz where

import           Prologue
import           Data.Binary                   (Binary)
import           Data.Text.Lazy                (Text)

import           Empire.API.Data.GraphLocation (GraphLocation)
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Topic              as T

data Request = Request { _location :: GraphLocation
                       } deriving (Generic, Show, Eq)

makeLenses ''Request

instance Binary Request

instance G.GraphRequest Request where location = location

topicPrefix = "empire.environment.debug.graphviz"
instance T.MessageTopic Request  where topic _ = topicPrefix <> T.request
