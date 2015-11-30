module Empire.Data.Project where

import Prologue
import Empire.Data.Library (Library)
import System.Path         (Path)

type ProjectId = Int

data Project = Project { _name     :: Maybe String
                       , _path     :: Path
                       , _libPaths :: [Path]
                       , _libs     :: [Library]
                       } deriving (Show)

make :: Maybe String -> Path -> Project
make name path = Project name path [] []

makeLenses ''Project
