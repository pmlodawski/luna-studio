module Empire.Data.Project where

import           Prologue
import           Empire.Data.Library (Library)
import           System.Path         (Path)
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap

type ProjectId = Int

data Project = Project { _name     :: Maybe String
                       , _path     :: Path
                       , _libs     :: IntMap Library
                       } deriving (Show)

make :: Maybe String -> Path -> Project
make name path = Project name path IntMap.empty

makeLenses ''Project
