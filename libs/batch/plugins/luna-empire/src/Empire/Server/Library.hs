{-# LANGUAGE OverloadedStrings #-}

module Empire.Server.Library where

import           Prologue

import           Control.Monad.State              (StateT)
import qualified Data.Binary                      as Bin
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Lazy             (fromStrict)
import qualified Empire.API.Library.CreateLibrary as CreateLibrary
import qualified Empire.API.Library.ListLibraries as ListLibraries
import qualified Empire.API.Topic                 as Topic
import qualified Empire.API.Response              as Response
import qualified Empire.Commands.Library          as Library
import qualified Empire.Data.Library              as DataLibrary
import qualified Empire.Empire                    as Empire
import           Empire.Env                       (Env)
import qualified Empire.Env                       as Env
import           Empire.Server.Server             (errorMessage, sendToBus', replyFail, replyResult)
import           Flowbox.Bus.BusT                 (BusT (..))
import qualified Flowbox.System.Log.Logger        as Logger

logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)


handleCreateLibrary :: CreateLibrary.Request -> StateT Env BusT ()
handleCreateLibrary request = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Library.createLibrary
        (request ^. CreateLibrary.projectId)
        (request ^. CreateLibrary.libraryName)
        (fromString $ request ^. CreateLibrary.path)
    case result of
        Left err -> replyFail logger err request
        Right (libraryId, library) -> do
            Env.empireEnv .= newEmpireEnv
            replyResult request $ CreateLibrary.Result libraryId $ DataLibrary.toAPI library
            sendToBus' $ CreateLibrary.Update libraryId $ DataLibrary.toAPI library

handleListLibraries :: ListLibraries.Request -> StateT Env BusT ()
handleListLibraries request = do
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Library.listLibraries
        (request ^. ListLibraries.projectId)
    case result of
        Left err -> replyFail logger err request
        Right librariesList -> do
            Env.empireEnv .= newEmpireEnv
            replyResult request $ ListLibraries.Result $ (_2 %~ DataLibrary.toAPI) <$> librariesList
