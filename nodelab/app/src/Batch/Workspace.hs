module Batch.Workspace where

import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.IntMap.Lazy              (IntMap)
import qualified Data.IntMap.Lazy              as IntMap
import           Utils.PreludePlus

import           Empire.API.Data.Breadcrumb    (Breadcrumb (..))
import           Empire.API.Data.GraphLocation (GraphLocation (..))
import qualified Empire.API.Data.GraphLocation as GraphLocation
import           Empire.API.Data.Library       (Library, LibraryId)
import qualified Empire.API.Data.Library       as Library
import           Empire.API.Data.Project       (Project, ProjectId)
import qualified Empire.API.Data.Project       as Project
import           Empire.API.JSONInstances      ()

import           Text.ScopeSearcher.Item       (Items)

data UIGraphLocation = UIGraphLocation { _projectName :: String
                                       , _libraryName :: String
                                       , _breadcrumb  :: Breadcrumb
                                       } deriving (Show, Eq, Generic)


data Workspace = Workspace { _projects         :: IntMap Project
                           , _currentLocation  :: GraphLocation
                           , _lastUILocation   :: Maybe UIGraphLocation
                           , _isGraphLoaded    :: Bool
                           , _nodeSearcherData :: Items
                           } deriving (Show, Eq, Generic)

instance ToJSON Workspace

instance Default Workspace where
    def = Workspace def (GraphLocation 0 0 (Breadcrumb [])) def False def

makeLenses ''Workspace

currentProject' :: Workspace -> Project
currentProject' w = fromMaybe err $ w ^? projects . ix id where
    id = w ^. currentLocation . GraphLocation.projectId
    err = error "Invalid project id"

currentProject :: Getter Workspace Project
currentProject = to currentProject'

currentLibrary' :: Workspace -> Library
currentLibrary' w = fromMaybe err $ project ^? Project.libs . ix id where
    id = w ^. currentLocation . GraphLocation.libraryId
    project = w ^. currentProject
    err = error "Invalid library id"

currentLibrary :: Getter Workspace Library
currentLibrary = to currentLibrary'


makeLenses ''UIGraphLocation
instance ToJSON UIGraphLocation
instance FromJSON UIGraphLocation

uiGraphLocation' :: Workspace -> UIGraphLocation
uiGraphLocation' w = UIGraphLocation project library breadcrumb where
    breadcrumb = w ^. currentLocation . GraphLocation.breadcrumb
    project    = w ^. currentProject  . Project.path
    library    = w ^. currentLibrary  . Library.path

uiGraphLocation :: Getter Workspace UIGraphLocation
uiGraphLocation = to uiGraphLocation'

fromUIGraphLocation :: [(ProjectId, Project)] -> UIGraphLocation -> Maybe GraphLocation
fromUIGraphLocation projs (UIGraphLocation proj lib bc) = do
    (projectId, project) <- find (\(_,p) -> p ^. Project.path == proj) projs
    let libs  = IntMap.toList $ project ^. Project.libs
    (libraryId, _) <- find (\(_,p) -> p ^. Library.path == lib) libs
    return $ GraphLocation projectId libraryId bc
