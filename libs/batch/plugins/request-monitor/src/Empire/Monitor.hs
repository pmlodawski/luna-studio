{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Empire.Monitor where

import           Control.Monad                     (forever)
import           Control.Monad.State               (StateT, evalStateT)
import qualified Data.Binary                       as Bin
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Char8             (unpack)
import           Data.ByteString.Lazy              (fromStrict, toStrict)
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Prologue

import qualified Empire.API.Control.EmpireStarted  as EmpireStarted
import qualified Empire.API.Graph.AddNode          as AddNode
import qualified Empire.API.Graph.CodeUpdate       as CodeUpdate
import qualified Empire.API.Graph.Connect          as Connect
import qualified Empire.API.Graph.Disconnect       as Disconnect
import qualified Empire.API.Graph.GetProgram       as GetProgram
import qualified Empire.API.Graph.NodeResultUpdate as NodeResultUpdate
import qualified Empire.API.Graph.NodeUpdate       as NodeUpdate
import qualified Empire.API.Graph.RemoveNode       as RemoveNode
import qualified Empire.API.Graph.RenameNode       as RenameNode
import qualified Empire.API.Graph.SetDefaultValue  as SetDefaultValue
import qualified Empire.API.Graph.UpdateNodeMeta   as UpdateNodeMeta
import qualified Empire.API.Graph.TypeCheck        as TypeCheck
import qualified Empire.API.Graph.DumpGraphViz     as DumpGraphViz
import qualified Empire.API.Library.CreateLibrary  as CreateLibrary
import qualified Empire.API.Library.ListLibraries  as ListLibraries
import qualified Empire.API.Project.CreateProject  as CreateProject
import qualified Empire.API.Project.ListProjects   as ListProjects
import qualified Empire.API.Project.ImportProject  as ImportProject
import qualified Empire.API.Project.ExportProject  as ExportProject
import qualified Empire.API.Topic                  as Topic
import           Empire.API.Request                (Request)
import qualified Empire.Commands.Library           as Library
import qualified Empire.Commands.Project           as Project
import qualified Empire.Empire                     as Empire
import           Empire.Env                        (MonitorEnv)
import qualified Empire.Env                        as Env
import qualified Empire.Utils                      as Utils

import qualified Flowbox.Bus.Bus                   as Bus
import           Flowbox.Bus.BusT                  (BusT (..))
import qualified Flowbox.Bus.BusT                  as Bus
import qualified Flowbox.Bus.Data.Message          as Message
import           Flowbox.Bus.Data.MessageFrame     (MessageFrame (MessageFrame))
import           Flowbox.Bus.Data.Topic            (Topic)
import           Flowbox.Bus.EndPoint              (BusEndPoints)
import qualified Flowbox.System.Log.Logger         as Logger


defaultTopic :: String
defaultTopic = "empire."

logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)

run :: BusEndPoints -> FilePath -> Int -> String -> IO (Either Bus.Error ())
run endPoints projectRoot time script = do
    logger Logger.info $ "Monitor: Script " <> script <> " scheduled to run after " <> show time <> " seconds of requests inactivity."
    -- run' endPoints [".request"]
    run' endPoints ["empire."]

run' :: BusEndPoints -> [Topic] -> IO (Either Bus.Error ())
run' endPoints topics = Bus.runBus endPoints $ do
    let formatted = True
    logger Logger.info $ "Subscribing to topics: " <> show topics
    logger Logger.info $ (Utils.display formatted) endPoints
    mapM_ Bus.subscribe topics
    Bus.runBusT $ evalStateT (runBus formatted) def

runBus :: Bool -> StateT MonitorEnv BusT ()
runBus formatted = do
    Env.formatLog .= formatted
    forever handleMessage

handleMessage :: StateT MonitorEnv BusT ()
handleMessage = do
    msgFrame <- lift $ BusT Bus.receive'
    case msgFrame of
        Left err -> logger Logger.error $ "Unparseable message: " <> err
        Right (MessageFrame msg crlID senderID lastFrame) -> do
            let topic = msg ^. Message.topic
                logMsg = show (crlID ^. Message.messageID) <> ": " <> show senderID
                         <> " -> (last = " <> show lastFrame
                         <> ")\t:: " <> topic
                content = msg ^. Message.message
                errorMsg = show content
            case Utils.lastPart '.' topic of
                "request"  -> logMessage logMsg topic content
                _          -> return ()
                
logMessage :: String -> String -> ByteString -> StateT MonitorEnv BusT ()
logMessage logMsg topic content = do
    putStrLn $ "Monitor " <> logMsg <> " topic " <> topic
    return ()
