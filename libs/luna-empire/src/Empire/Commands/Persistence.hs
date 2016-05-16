module Empire.Commands.Persistence
    ( saveProject
    , saveLocation
    , loadProject
    , loadAllProjects
    , ensureDefaultExists
    ) where

import           Control.Monad.Error             (throwError)
import           Control.Monad.Reader
import           Control.Monad.State
import qualified Data.ByteString.Lazy            as BS (ByteString, putStrLn, readFile, writeFile)
import qualified Data.IntMap                     as IntMap
import           Data.Maybe                      (fromMaybe)
import           Prologue
import           System.Path                     (Path)

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

import qualified Data.Text.Lazy                  as Text
import           Empire.API.JSONInstances        ()
import           System.Path                     (Path, native)


import System.FilePath
import System.FilePath.Find
import System.FilePath.Glob
import System.FilePath.Manip

import qualified Flowbox.System.Log.Logger         as Logger

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
serialize = JSON.encode

saveProject :: FilePath -> ProjectId -> Empire ()
saveProject projectRoot pid = do
  project <- toPersistentProject pid
  let bytes = serialize $ E.pack project
      path  = projectRoot <> "/" <> (fromMaybe "untitled" $ project ^. P.name) <> ".lproj"

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


createProjectFromPersistent :: P.Project -> Empire ProjectId
createProjectFromPersistent p = do
  (pid, _) <- createProject (p ^. P.name) (fromString $ p ^. P.path)

  forM_ (p ^. P.libs) $ \lib -> do
    (lid, _) <- createLibrary pid (lib ^. L.name) (fromString $ lib ^. L.path)
    withLibrary pid lid $ zoom Library.body $ do
      let graph = lib ^. L.graph
          nodes = graph ^. G.nodes
          connections = graph ^. G.connections
      mapM Graph.addPersistentNode nodes
      mapM (uncurry Graph.connectNoTC) connections
  return pid


loadProject :: FilePath -> Empire (Maybe ProjectId)
loadProject path = do
  logger Logger.info $ "Loading project " <> path
  proj <- liftIO $ readProject path
  mapM createProjectFromPersistent proj

projectFiles :: FilePath -> IO [FilePath]
projectFiles = find always (extension ==? ".lproj")

loadAllProjects :: FilePath -> Empire [ProjectId]
loadAllProjects root = do
  projects <- liftIO $ projectFiles root
  loadedProjects <- mapM loadProject projects
  return $ catMaybes loadedProjects

defaultProjectName = "default project"
defaultProjectPath = "default_project"
defaultLibraryName = "Main"
defaultLibraryPath = "Main.luna"

ensureDefaultExists :: [ProjectId] -> Empire ()
ensureDefaultExists [] = do
  logger Logger.info "Creating default project"
  (projectId, _) <- createProject (Just defaultProjectName) (fromString defaultProjectPath)
  void $ createLibrary projectId (Just defaultLibraryName) (fromString defaultLibraryPath)
ensureDefaultExists _ = return ()


