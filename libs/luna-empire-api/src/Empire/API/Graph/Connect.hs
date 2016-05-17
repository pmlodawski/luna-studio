module Empire.API.Graph.Connect where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.GraphLocation (GraphLocation)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.Port          (OutPort, InPort)
import           Empire.API.Data.PortRef       (OutPortRef(..), InPortRef(..))
import qualified Empire.API.Response             as Response
import qualified Empire.API.Graph.Request      as G

data Request = Request { _location  :: GraphLocation
                       , _src       :: OutPortRef
                       , _dst       :: InPortRef
                       } deriving (Generic, Show, Eq)

type Response = Response.SimpleResponse Request

data Update  = Update  { _location'  :: GraphLocation
                       , _src'       :: OutPortRef
                       , _dst'       :: InPortRef
                       } deriving (Generic, Show, Eq)


makeLenses ''Request
makeLenses ''Update
instance Binary Request
instance Binary Update

instance G.GraphRequest Request where location = location