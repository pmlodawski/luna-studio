module Empire.Data.Project where

import Prologue
import Empire.Data.Library    (Library)
import Flowbox.System.UniPath (UniPath)

data Project = Project { _name     :: Maybe String
                       , _path     :: UniPath
                       , _libPaths :: [UniPath]
                       , _libs     :: [Library]
                       } deriving (Show, Generic)

makeLenses ''Project
