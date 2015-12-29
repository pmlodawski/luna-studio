module Empire.API.Graph.Connect where

import Prologue
import Empire.Objects.Project (ProjectId)
import Empire.Objects.Library (LibraryId)
import Empire.Objects.Node    (NodeId)
import Empire.Objects.Port    (OutPort, InPort)

data Connect = Connect { _projectId :: ProjectId
                       , _libraryId :: LibraryId
                       , _srcNodeId :: NodeId
                       -- , _srcPort   :: OutPort  -- not used
                       , _dstNodeId :: NodeId
                       , _dstPort   :: InPort
                       } deriving (Show, Eq)

makeLenses ''Connect
