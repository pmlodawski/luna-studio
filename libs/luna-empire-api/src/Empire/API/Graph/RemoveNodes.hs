module Empire.API.Graph.RemoveNodes where

import           Data.Aeson                    (ToJSON)
import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import qualified Empire.API.Response           as Response
import qualified Empire.API.Graph.Request      as G
import qualified Empire.API.Topic              as T
import qualified Empire.API.Request            as R

data Request = Request { _location :: GraphLocation
                       , _nodeIds  :: [NodeId]
                       } deriving (Generic, Show, Eq)

type Response = Response.SimpleResponse Request
instance Response.ResponseResult Request ()

data Update  = Update  { _location' :: GraphLocation
                       , _nodeIds'  :: [NodeId]
                       } deriving (Generic, Show, Eq)


makeLenses ''Request
makeLenses ''Update

instance Binary Request
instance Binary Update

instance ToJSON Request
instance ToJSON Update

instance G.GraphRequest Request where location = location

topicPrefix = "empire.graph.node.remove"
instance T.MessageTopic (R.Request Request)  where topic _ = topicPrefix <> T.request
instance T.MessageTopic Response where topic _ = topicPrefix <> T.response
instance T.MessageTopic Update   where topic _ = topicPrefix <> T.update
