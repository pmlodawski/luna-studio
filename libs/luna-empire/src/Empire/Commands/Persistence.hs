module Empire.Commands.Persistence
    ( saveProject
    , saveLocation
    , loadProject
    , createDefaultProject
    ) where

import           Control.Monad.Error             (throwError)
import           Control.Monad.Error             (catchError)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Lazy            as BS (ByteString, putStrLn, readFile, writeFile)
import qualified Data.IntMap                     as IntMap
import           Data.Maybe                      (fromMaybe)
import qualified Data.UUID                       as UUID
import           Prologue
import           System.Path                     (Path)
import           System.FilePath                 (takeBaseName)

import           Empire.Data.Library             (Library)
import qualified Empire.Data.Library             as Library
import           Empire.Data.Project             (Project)
import qualified Empire.Data.Project             as Project

import qualified Empire.API.Data.Graph           as G
import           Empire.API.Data.GraphLocation   (GraphLocation (..))
import           Empire.API.Data.Library         (LibraryId)
import           Empire.API.Data.Project         (ProjectId)
import qualified Empire.API.Persistence.Envelope as E
import qualified Empire.API.Persistence.Library  as L
import qualified Empire.API.Persistence.Project  as P

import qualified Empire.Commands.Graph           as Graph
import           Empire.Commands.GraphBuilder    (buildGraph)
import           Empire.Commands.Library         (createLibrary, listLibraries, withLibrary)
import           Empire.Commands.Project         (createProject, withProject)
import           Empire.Empire                   (Command, Empire)
import qualified Empire.Empire                   as Empire
import qualified Empire.Utils.IdGen              as IdGen

import qualified Data.Aeson                      as JSON
import qualified Data.Aeson.Encode.Pretty        as JSON

import qualified Data.Text.Lazy                  as Text
import           Empire.API.JSONInstances        ()
import           System.Path                     (Path, native)

import qualified Flowbox.System.Log.Logger       as Logger

logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)


toPersistentProject :: ProjectId -> Empire P.Project
toPersistentProject pid = do
  libs <- listLibraries pid
  almostProject <- withProject pid $ do
    proj <- get
    return $ Project.toPersistent proj

  libs' <- forM (libs) $ \(lid, lib) -> do
    graph <- withLibrary pid lid . zoom Library.body $ buildGraph
    return $ (lid, Library.toPersistent lib graph)

  return $ almostProject $ IntMap.fromList libs'

serialize :: E.Envelope -> BS.ByteString
serialize = JSON.encodePretty

saveProject :: FilePath -> ProjectId -> Empire ()
saveProject projectRoot pid = do
  project <- toPersistentProject pid
  let bytes = serialize $ E.pack project
      path  = projectRoot <> "/" <> (UUID.toString pid) <> ".lproj"

  logger Logger.info $ "Saving project " <> path
  liftIO $ BS.writeFile path bytes


saveLocation :: FilePath -> GraphLocation -> Empire ()
saveLocation projectRoot (GraphLocation pid _ _) = saveProject projectRoot pid

readProject :: FilePath -> IO (Maybe P.Project)
readProject filename = do
  bytes   <- BS.readFile filename
  -- TODO: migrate data
  let envelope = JSON.decode bytes
  return $ (view E.project) <$> envelope


createProjectFromPersistent :: Maybe ProjectId -> P.Project -> Empire ProjectId
createProjectFromPersistent maybePid p = do
  (pid, _) <- createProject maybePid (p ^. P.name)

  forM_ (p ^. P.libs) $ \lib -> do
    (lid, _) <- createLibrary pid (lib ^. L.name) (fromString $ lib ^. L.path)
    withLibrary pid lid $ zoom Library.body $ do
      let graph = lib ^. L.graph
          nodes = graph ^. G.nodes
          connections = graph ^. G.connections
      mapM Graph.addPersistentNode nodes
      mapM (uncurry Graph.connectNoTC) connections
  return pid


loadProject :: FilePath -> Empire ProjectId
loadProject path = do
    logger Logger.info $ "Loading project " <> path
    proj <- liftIO $ readProject path
    let basename = takeBaseName path
        maybeProjId = UUID.fromString basename
    case proj of
      Nothing   -> throwError $ "Cannot read JSON from " <> path
      Just proj -> createProjectFromPersistent maybeProjId proj

defaultProjectName = "default"
defaultLibraryName = "Main"
defaultLibraryPath = "Main.luna"

createDefaultProject :: Empire ()
createDefaultProject = do
  logger Logger.info "Creating default project"
  (projectId, _) <- createProject Nothing defaultProjectName
  void $ createLibrary projectId (Just defaultLibraryName) (fromString defaultLibraryPath)


