module Empire.API.Graph.Connect where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.Port          (OutPort, InPort)
import           Empire.API.Data.PortRef       (OutPortRef(..), InPortRef(..))
import qualified Empire.API.Response             as Response
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Topic              as T

data Request = Request { _location  :: GraphLocation
                       , _src       :: OutPortRef
                       , _dst       :: InPortRef
                       } deriving (Generic, Show, Eq)

type Response = Response.SimpleResponse Request
instance Response.ResponseResult Request ()

data Update  = Update  { _location'  :: GraphLocation
                       , _src'       :: OutPortRef
                       , _dst'       :: InPortRef
                       } deriving (Generic, Show, Eq)


makeLenses ''Request
makeLenses ''Update
instance Binary Request
instance Binary Update

instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.connect"
instance T.MessageTopic Request  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
