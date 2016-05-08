module Empire.Data.Project where

import           Prologue
import qualified Data.Text.Lazy          as Text
import           Empire.Data.Library     (Library)
import qualified Empire.Data.Library     as Library
import qualified Empire.API.Data.Project as API
import qualified Empire.API.Persistence.Project as Persistence
import qualified Empire.API.Persistence.Library as Persistence
import           System.Path             (Path, native)
import           Data.IntMap             (IntMap)
import qualified Data.IntMap             as IntMap

data Project = Project { _name     :: Maybe String
                       , _path     :: Path
                       , _libs     :: IntMap Library
                       } deriving (Show)

make :: Maybe String -> Path -> Project
make name path = Project name path IntMap.empty

makeLenses ''Project

toAPI :: Project -> API.Project
toAPI (Project name path libs) = API.Project name (Text.unpack $ native path) (Library.toAPI <$> libs)

toPersistent :: Project -> IntMap Persistence.Library -> Persistence.Project
toPersistent (Project name path _) = Persistence.Project name (Text.unpack $ native path)
