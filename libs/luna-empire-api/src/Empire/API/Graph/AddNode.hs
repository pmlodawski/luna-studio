{-# LANGUAGE DeriveGeneric #-}

module Empire.API.Graph.AddNode where

import Prologue
import Data.Binary             (Binary)

import Empire.API.Data.Project (ProjectId)
import Empire.API.Data.Library (LibraryId)

data AddNode = AddNode { _projectId :: ProjectId
                       , _libraryId :: LibraryId
                       , _expr      :: String
                       } deriving (Generic, Show, Eq)

makeLenses ''AddNode

instance Binary AddNode
