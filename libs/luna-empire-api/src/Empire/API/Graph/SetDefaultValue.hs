module Empire.API.Graph.SetDefaultValue where

import           Prologue
import           Data.Binary                   (Binary)

import           Empire.API.Data.Project       (ProjectId)
import           Empire.API.Data.Library       (LibraryId)
import           Empire.API.Data.Node          (NodeId)
import           Empire.API.Data.Port          (InPort)
import           Empire.API.Data.DefaultValue  (PortDefault)
import qualified Empire.API.Response           as Response

data Request = Request { _projectId    :: ProjectId
                       , _libraryId    :: LibraryId
                       , _nodeId       :: NodeId
                       , _portId       :: InPort
                       , _defaultValue :: PortDefault
                       } deriving (Generic, Show, Eq)

type Response = Response.SimpleResponse Request

makeLenses ''Request

instance Binary Request
