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
import qualified Empire.API.Response              as Response
import           Empire.API.Request                (Request(..))
import qualified Empire.Commands.Library          as Library
import qualified Empire.Commands.Project          as Project
import qualified Empire.Data.Project              as DataProject
import qualified Empire.Empire                    as Empire
import           Empire.Env                       (Env)
import qualified Empire.Env                       as Env
import           Empire.Server.Server             (errorMessage, sendToBus', replyFail, replyResult)
import           Flowbox.Bus.BusT                 (BusT (..))
import qualified Flowbox.System.Log.Logger        as Logger

logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)


handleCreateProject :: Request CreateProject.Request -> StateT Env BusT ()
handleCreateProject req@(Request _ request) = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ do
      (projectId, project) <- Project.createProject Nothing (request ^. CreateProject.name)
      (libraryId, library) <- Library.createLibrary projectId (Just "Main") "Main.luna"

      let project' = project & DataProject.libs . at libraryId ?~ library
      return (projectId, project')
    case result of
        Left err -> replyFail logger err req
        Right (projectId, project) -> do
            Env.empireEnv .= newEmpireEnv
            replyResult req $ CreateProject.Result projectId $ DataProject.toAPI project
            sendToBus' $ CreateProject.Update projectId $ DataProject.toAPI project

handleListProjects :: Request ListProjects.Request -> StateT Env BusT ()
handleListProjects req = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Project.listProjects
    case result of
        Left err -> replyFail logger err req
        Right projectList -> do
            Env.empireEnv .= newEmpireEnv
            replyResult req $ ListProjects.Result $ (_2 %~ DataProject.toAPI) <$> projectList
