module Empire.API.Graph.UpdateNode where

import           Prologue
import           Data.Binary              (Binary)

import           Empire.API.Data.Project  (ProjectId)
import           Empire.API.Data.Library  (LibraryId)
import           Empire.API.Data.Node     (Node)

data Update = Update { _projectId :: ProjectId
                     , _libraryId :: LibraryId
                     , _node      :: Node
                     } deriving (Generic, Show, Eq)

makeLenses ''Update

instance Binary Update
