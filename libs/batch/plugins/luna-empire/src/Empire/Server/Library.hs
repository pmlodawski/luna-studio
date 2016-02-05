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
import qualified Empire.API.Update                as Update
import qualified Empire.Commands.Library          as Library
import qualified Empire.Data.Library              as DataLibrary
import qualified Empire.Empire                    as Empire
import           Empire.Env                       (Env)
import qualified Empire.Env                       as Env
import           Empire.Server.Server             (errorMessage, sendToBus)
import           Flowbox.Bus.BusT                 (BusT (..))
import qualified Flowbox.System.Log.Logger        as Logger

logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)

handleCreateLibrary :: ByteString -> StateT Env BusT ()
handleCreateLibrary content = do
    let request = Bin.decode . fromStrict $ content :: CreateLibrary.Request
    currentEmpireEnv <- use Env.empireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Library.createLibrary
        (request ^. CreateLibrary.projectId)
        (request ^. CreateLibrary.libraryName)
        (fromString $ request ^. CreateLibrary.path)
    case result of
        Left err -> logger Logger.error $ errorMessage <> err
        Right (libraryId, library) -> do
            Env.empireEnv .= newEmpireEnv
            let update = Update.Update request $ CreateLibrary.Result libraryId $ DataLibrary.toAPI library
            sendToBus Topic.createLibraryUpdate update

handleListLibraries :: ByteString -> StateT Env BusT ()
handleListLibraries content = do
    let request = Bin.decode . fromStrict $ content :: ListLibraries.Request
    currentEmpireEnv <- use Env.empireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Library.listLibraries
        (request ^. ListLibraries.projectId)
    case result of
        Left err -> logger Logger.error $ errorMessage <> err
        Right librariesList -> do
            Env.empireEnv .= newEmpireEnv
            let update = Update.Update request $ ListLibraries.Status $ (_2 %~ DataLibrary.toAPI) <$> librariesList
            sendToBus Topic.listLibrariesStatus update
