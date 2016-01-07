module Empire.API.Graph.Connect where

import           Prologue
import           Data.Binary             (Binary)

import           Empire.API.Data.Project (ProjectId)
import           Empire.API.Data.Library (LibraryId)
import           Empire.API.Data.Node    (NodeId)
import           Empire.API.Data.Port    (OutPort, InPort)
import qualified Empire.API.Response     as Response

data Request = Request { _projectId :: ProjectId
                       , _libraryId :: LibraryId
                       , _srcNodeId :: NodeId
                       , _srcPort   :: OutPort
                       , _dstNodeId :: NodeId
                       , _dstPort   :: InPort
                       } deriving (Generic, Show, Eq)

type Response = Response.SimpleResponse Request

makeLenses ''Request

instance Binary Request
