{-# LANGUAGE OverloadedStrings #-}

module Empire.Server.Library where

import           Prologue

import qualified Data.Binary                      as Bin
import           System.Path                      (Path)
import           Control.Monad.State              (StateT, get, put)
import           Data.Map.Strict                  (Map)
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Char8            (unpack)
import           Data.ByteString.Lazy             (fromStrict, toStrict)
import           Data.Text.Lazy                   (Text)
import qualified Data.Text.Lazy                   as Text
import qualified Flowbox.Bus.Data.Flag            as Flag
import qualified Flowbox.Bus.Data.Message         as Message
import qualified Flowbox.Bus.Bus                  as Bus
import           Flowbox.Bus.BusT                 (BusT (..))
import qualified Flowbox.Bus.BusT                 as Bus
import qualified Flowbox.System.Log.Logger        as Logger
import qualified Empire.Env                       as Env
import           Empire.Env                       (Env)
import qualified Empire.Data.Library              as DataLibrary
import           Empire.Data.AST                  (AST)
import qualified Empire.API.Library.CreateLibrary as CreateLibrary
import qualified Empire.API.Library.ListLibraries as ListLibraries
import qualified Empire.API.Update                as Update
import qualified Empire.API.Topic                 as Topic
import qualified Empire.Commands.Library          as LibraryCmd
import qualified Empire.Empire                    as Empire
import           Empire.Empire                    (Empire)
import qualified Empire.Server.Server             as Server

logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)

handleCreateLibrary :: ByteString -> StateT Env BusT ()
handleCreateLibrary content = do
    let request = Bin.decode . fromStrict $ content :: CreateLibrary.Request
    currentEmpireEnv <- use Env.empireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ LibraryCmd.createLibrary
        (request ^. CreateLibrary.projectId)
        (request ^. CreateLibrary.libraryName)
        (fromString $ request ^. CreateLibrary.path)
    case result of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right (libraryId, library) -> do
            Env.empireEnv .= newEmpireEnv
            let response = Update.Update request $ CreateLibrary.Result libraryId $ DataLibrary.toAPI library
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.createLibraryUpdate $ toStrict $ Bin.encode response
            return ()

handleListLibraries :: ByteString -> StateT Env BusT ()
handleListLibraries content = do
    let request = Bin.decode . fromStrict $ content :: ListLibraries.Request
    currentEmpireEnv <- use Env.empireEnv
    (result, newEmpireEnv) <- liftIO $ Empire.runEmpire currentEmpireEnv $ LibraryCmd.listLibraries
        (request ^. ListLibraries.projectId)
    case result of
        Left err -> logger Logger.error $ Server.errorMessage ++ err
        Right librariesList -> do
            Env.empireEnv .= newEmpireEnv
            let librariesListAPI = fmap (\(libraryId, library) -> (libraryId, DataLibrary.toAPI library)) librariesList
                response = Update.Update request $ ListLibraries.Status librariesListAPI
            lift $ BusT $ Bus.send Flag.Enable $ Message.Message Topic.listLibrariesStatus $ toStrict $ Bin.encode response
            return ()
