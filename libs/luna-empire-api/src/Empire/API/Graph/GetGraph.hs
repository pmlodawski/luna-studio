module Empire.API.Graph.GetGraph where

import           Prologue
import           Data.Binary              (Binary)

import           Empire.API.Data.Project  (ProjectId)
import           Empire.API.Data.Library  (LibraryId)
import           Empire.API.Data.Node     (Node)
import           Empire.API.Data.PortRef  (InPortRef, OutPortRef)
import qualified Empire.API.Response      as Response

data Request = Request { _projectId :: ProjectId
                       , _libraryId :: LibraryId
                       } deriving (Generic, Show, Eq)

data Update = Update { _nodes       :: [Node]
                     , _connections :: [(OutPortRef, InPortRef)]
                     } deriving (Generic, Show, Eq)

type Response = Response.Response Request Update

makeLenses ''Request
makeLenses ''Update

instance Binary Request
instance Binary Update
