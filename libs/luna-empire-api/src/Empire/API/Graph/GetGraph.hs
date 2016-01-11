module Empire.API.Graph.GetGraph where

import           Prologue
import           Data.Binary              (Binary)

import           Empire.API.Data.Project  (ProjectId)
import           Empire.API.Data.Library  (LibraryId)
import           Empire.API.Data.Node     (Node)
import           Empire.API.Data.Graph    (Graph)
import           Empire.API.Data.PortRef  (InPortRef, OutPortRef)
import qualified Empire.API.Response      as Response

data Request = Request { _projectId :: ProjectId
                       , _libraryId :: LibraryId
                       } deriving (Generic, Show, Eq)

data Status = Status { _graph       :: Graph
                     } deriving (Generic, Show, Eq)

type Response = Response.Response Request Status

makeLenses ''Request
makeLenses ''Status

instance Binary Request
instance Binary Update
