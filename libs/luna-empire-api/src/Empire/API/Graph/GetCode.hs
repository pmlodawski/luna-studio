module Empire.API.Graph.GetCode where

import           Prologue
import           Data.Binary                   (Binary)
import           Data.Text.Lazy (Text)

import           Empire.API.Data.GraphLocation (GraphLocation)
import qualified Empire.API.Response           as Response

data Request = Request { _location :: GraphLocation
                       } deriving (Generic, Show, Eq)

data Status = Status { _code :: Text
                     } deriving (Generic, Show, Eq)

type Response = Response.Response Request Status

makeLenses ''Request
makeLenses ''Status

instance Binary Request
instance Binary Status
