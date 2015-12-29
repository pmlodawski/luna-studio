{-# LANGUAGE DeriveGeneric #-}

module Empire.API.Graph.Disconnect where

import Prologue
import Data.Binary             (Binary)

import Empire.API.Data.Project (ProjectId)
import Empire.API.Data.Library (LibraryId)
import Empire.API.Data.Node    (NodeId)
import Empire.API.Data.Port    (OutPort, InPort)

data Disconnect = Disconnect { _projectId :: ProjectId
                             , _libraryId :: LibraryId
                             , _dstNodeId :: NodeId
                             , _dstPort   :: InPort
                             } deriving (Generic, Show, Eq)

makeLenses ''Disconnect

instance Binary Disconnect
