{-# LANGUAGE OverloadedStrings #-}

module Empire.Server.Project where

import           Prologue

import           Control.Monad.State              (StateT)
import qualified Data.Binary                      as Bin
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Lazy             (fromStrict)
import qualified Empire.API.Project.CreateProject as CreateProject
import qualified Empire.API.Project.ListProjects  as ListProjects
import qualified Empire.API.Topic                 as Topic
import qualified Empire.API.Update                as Update
import qualified Empire.Commands.Library          as Library
import qualified Empire.Commands.Project          as Project
import qualified Empire.Data.Project              as DataProject
import qualified Empire.Empire                    as Empire
import           Empire.Env                       (Env)
import qualified Empire.Env                       as Env
import           Empire.Server.Server             (errorMessage, sendToBus)
import           Flowbox.Bus.BusT                 (BusT (..))
import qualified Flowbox.System.Log.Logger        as Logger

logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)

handleCreateProject :: ByteString -> StateT Env BusT ()
handleCreateProject content = do
    let request = Bin.decode . fromStrict $ content :: CreateProject.Request
    currentEmpireEnv <- use Env.empireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ do
      (projectId, project) <- Project.createProject (request ^. CreateProject.projectName) (fromString $ request ^. CreateProject.path)
      (libraryId, library) <- Library.createLibrary projectId (Just "Main") "Main.luna"

      let project' = project & DataProject.libs . at libraryId ?~ library
      return (projectId, project')
    case result of
        Left err -> logger Logger.error $ errorMessage <> err
        Right (projectId, project) -> do
            Env.empireEnv .= newEmpireEnv
            let update = Update.Update request $ CreateProject.Result projectId $ DataProject.toAPI project
            sendToBus Topic.createProjectUpdate update

handleListProjects :: ByteString -> StateT Env BusT ()
handleListProjects content = do
    let request = Bin.decode . fromStrict $ content :: ListProjects.Request
    currentEmpireEnv <- use Env.empireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Project.listProjects
    case result of
        Left err -> logger Logger.error $ errorMessage <> err
        Right projectList -> do
            Env.empireEnv .= newEmpireEnv
            let update = Update.Update request $ ListProjects.Status $ (_2 %~ DataProject.toAPI) <$> projectList
            sendToBus Topic.listProjectsStatus update
