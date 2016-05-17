module Empire.API.Graph.RemoveNode where

import           Data.Binary                   (Binary)
import           Prologue

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import qualified Empire.API.Response           as Response
import qualified Empire.API.Graph.Request      as G

data Request = Request { _location :: GraphLocation
                       , _nodeIds  :: [NodeId]
                       } deriving (Generic, Show, Eq)

type Response = Response.SimpleResponse Request

data Update  = Update  { _location' :: GraphLocation
                       , _nodeIds'  :: [NodeId]
                       } deriving (Generic, Show, Eq)


makeLenses ''Request
instance Binary Request
makeLenses ''Update
instance Binary Update
instance G.GraphRequest Request where location = location
