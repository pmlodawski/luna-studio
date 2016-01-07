module Empire.Data.Project where

import           Prologue
import           Empire.Data.Library (Library)
import qualified Empire.Data.Library as Library
import qualified Empire.API.Data.Project as API
import           System.Path         (Path)
import           Data.IntMap         (IntMap)
import qualified Data.IntMap         as IntMap

data Project = Project { _name     :: Maybe String
                       , _path     :: Path
                       , _libs     :: IntMap Library
                       } deriving (Show)

make :: Maybe String -> Path -> Project
make name path = Project name path IntMap.empty

makeLenses ''Project

toAPI :: Project -> API.Project
toAPI (Project name path libs) = API.Project name (show path) (Library.toAPI <$> libs)
