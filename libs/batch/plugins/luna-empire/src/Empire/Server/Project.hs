{-# LANGUAGE OverloadedStrings #-}

module Empire.Server.Project where

import           Prologue

import qualified Data.Binary                 as Bin
import           System.Path             (Path)
import           Control.Monad.State         (StateT, get, put)
import           Data.Map.Strict             (Map)
import           Data.ByteString             (ByteString)
import           Data.ByteString.Char8       (unpack)
import           Data.ByteString.Lazy        (fromStrict, toStrict)
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as Text
import qualified Flowbox.Bus.Data.Flag       as Flag
import qualified Flowbox.Bus.Data.Message    as Message
import qualified Flowbox.Bus.Bus             as Bus
import           Flowbox.Bus.BusT            (BusT (..))
import qualified Flowbox.Bus.BusT            as Bus
import           Flowbox.System.Log.Logger
import qualified Empire.Env                  as Env
import           Empire.Env                  (Env)
import           Empire.Data.Project         (Project)   -- should be in API
import           Empire.API.Data.Project     (ProjectId)
import qualified Empire.API.Response         as Response
import qualified Empire.Commands.Project     as ProjectCmd
import           Empire.Data.AST             (AST)
import qualified Empire.Empire               as Empire
import           Empire.Empire               (Empire)

logger :: LoggerIO
logger = getLoggerIO $moduleName


createProject :: Maybe String -> Path -> Empire (ProjectId, Project)
createProject name path = do
    (projectId, project) <- ProjectCmd.createProject name path
    return (projectId, project)

handleCreateProject :: ByteString -> StateT Env BusT ()
handleCreateProject content = do
    return ()
