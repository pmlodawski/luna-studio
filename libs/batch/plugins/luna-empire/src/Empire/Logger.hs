{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE RankNTypes       #-}

module Empire.Logger where

import           Prologue
import           Control.Monad                          (forever)
import           Control.Monad.State                    (StateT, evalStateT)
import qualified Data.Binary                            as Bin
import           Data.ByteString                        (ByteString)
import           Data.ByteString.Char8                  (unpack)
import           Data.ByteString.Lazy                   (fromStrict, toStrict)
import           Data.Map.Strict                        (Map)
import qualified Data.Map.Strict                        as Map
import qualified Empire.Env                             as Env
import           Empire.Env                             (Env)
import qualified Flowbox.Bus.Bus                        as Bus
import           Flowbox.Bus.BusT                       (BusT (..))
import qualified Flowbox.Bus.BusT                       as Bus
import qualified Flowbox.Bus.Data.Message               as Message
import           Flowbox.Bus.Data.MessageFrame          (MessageFrame (MessageFrame))
import           Flowbox.Bus.Data.Topic                 (Topic)
import           Flowbox.Bus.EndPoint                   (BusEndPoints)
import qualified Flowbox.System.Log.Logger              as Logger
import qualified Empire.Utils                           as Utils
import qualified Empire.Handlers                        as Handlers
import qualified Empire.Commands.Library                as Library
import qualified Empire.Commands.Project                as Project
import qualified Empire.Empire                          as Empire
import qualified Empire.Server.Server                   as Server
import qualified Empire.API.Topic                       as Topic
import qualified Empire.API.Graph.AddNode               as AddNode
import qualified Empire.API.Graph.RemoveNode            as RemoveNode
import qualified Empire.API.Graph.UpdateNodeMeta        as UpdateNodeMeta
import qualified Empire.API.Graph.Connect               as Connect
import qualified Empire.API.Graph.Disconnect            as Disconnect
import qualified Empire.API.Graph.GetProgram            as GetProgram
import qualified Empire.API.Graph.CodeUpdate            as CodeUpdate
import qualified Empire.API.Graph.NodeUpdate            as NodeUpdate
import qualified Empire.API.Graph.NodeResultUpdate      as NodeResultUpdate
import qualified Empire.API.Graph.SetDefaultValue       as SetDefaultValue
import qualified Empire.API.Project.CreateProject       as CreateProject
import qualified Empire.API.Project.ListProjects        as ListProjects
import qualified Empire.API.Library.CreateLibrary       as CreateLibrary
import qualified Empire.API.Library.ListLibraries       as ListLibraries


logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)

run :: BusEndPoints -> [Topic] -> Bool -> IO (Either Bus.Error ())
run endPoints topics formatted = Bus.runBus endPoints $ do
    logger Logger.info $ "Subscribing to topics: " <> show topics
    logger Logger.info $ show endPoints
    mapM_ Bus.subscribe topics
    Bus.runBusT $ evalStateT (runBus formatted) def

runBus :: Bool -> StateT Env BusT ()
runBus formatted = do
    Env.formatted .= formatted
    forever handleMessage

handleMessage :: StateT Env BusT ()
handleMessage = do
    msgFrame <- lift $ BusT Bus.receive'
    case msgFrame of
        Left err -> logger Logger.error $ "Unparseable message: " <> err
        Right (MessageFrame msg crlID senderID lastFrame) -> do
            let topic = msg ^. Message.topic
                logMsg =  show senderID <> " -> (last = " <> show lastFrame <> ")\t:: " <> topic
                content = msg ^. Message.message
                errorMsg = show content
            case Utils.lastPart '.' topic of
                "update"   -> logMessage logMsg topic content
                "status"   -> logMessage logMsg topic content
                "request"  -> logMessage logMsg topic content
                _          -> do logger Logger.error logMsg
                                 logger Logger.error errorMsg

type LogFormatter = (forall a. Show a => a -> String) -> ByteString -> String

logMessage :: String -> String -> ByteString -> StateT Env BusT ()
logMessage logMsg topic content = do
    formatted <- use Env.formatted
    logger Logger.info logMsg
    let logFormatter = Map.findWithDefault defaultLogFormatter topic loggFormattersMap
    logger Logger.debug $ logFormatter (Utils.display formatted) content

loggFormattersMap :: Map String LogFormatter
loggFormattersMap = Map.fromList
    [ (Topic.addNodeRequest,         \display content -> display (Bin.decode . fromStrict $ content :: AddNode.Request))
    , (Topic.addNodeUpdate,          \display content -> display (Bin.decode . fromStrict $ content :: AddNode.Update))
    , (Topic.removeNodeRequest,      \display content -> display (Bin.decode . fromStrict $ content :: RemoveNode.Request))
    , (Topic.removeNodeUpdate,       \display content -> display (Bin.decode . fromStrict $ content :: RemoveNode.Update))
    , (Topic.updateNodeMetaRequest,  \display content -> display (Bin.decode . fromStrict $ content :: UpdateNodeMeta.Request))
    , (Topic.updateNodeMetaUpdate,   \display content -> display (Bin.decode . fromStrict $ content :: UpdateNodeMeta.Update))
    , (Topic.connectRequest,         \display content -> display (Bin.decode . fromStrict $ content :: Connect.Request))
    , (Topic.connectUpdate,          \display content -> display (Bin.decode . fromStrict $ content :: Connect.Update))
    , (Topic.disconnectRequest,      \display content -> display (Bin.decode . fromStrict $ content :: Disconnect.Request))
    , (Topic.disconnectUpdate,       \display content -> display (Bin.decode . fromStrict $ content :: Disconnect.Update))
    , (Topic.programRequest,         \display content -> display (Bin.decode . fromStrict $ content :: GetProgram.Request))
    , (Topic.programStatus,          \display content -> display (Bin.decode . fromStrict $ content :: GetProgram.Update))
    , (Topic.nodeUpdate,             \display content -> display (Bin.decode . fromStrict $ content :: NodeUpdate.Update))
    , (Topic.nodeResultUpdate,       \display content -> display (Bin.decode . fromStrict $ content :: NodeResultUpdate.Update))
    , (Topic.codeUpdate,             \display content -> display (Bin.decode . fromStrict $ content :: CodeUpdate.Update))
    , (Topic.graphUpdate,            \display content -> "graphUpdate - not implemented yet")
    , (Topic.createProjectRequest,   \display content -> display (Bin.decode . fromStrict $ content :: CreateProject.Request))
    , (Topic.createProjectUpdate,    \display content -> display (Bin.decode . fromStrict $ content :: CreateProject.Update))
    , (Topic.listProjectsRequest,    \display content -> display (Bin.decode . fromStrict $ content :: ListProjects.Request))
    , (Topic.listProjectsStatus,     \display content -> display (Bin.decode . fromStrict $ content :: ListProjects.Update))
    , (Topic.createLibraryRequest,   \display content -> display (Bin.decode . fromStrict $ content :: CreateLibrary.Request))
    , (Topic.createLibraryUpdate,    \display content -> display (Bin.decode . fromStrict $ content :: CreateLibrary.Update))
    , (Topic.listLibrariesRequest,   \display content -> display (Bin.decode . fromStrict $ content :: ListLibraries.Request))
    , (Topic.listLibrariesStatus,    \display content -> display (Bin.decode . fromStrict $ content :: ListLibraries.Update))
    , (Topic.setDefaultValueRequest, \display content -> display (Bin.decode . fromStrict $ content :: SetDefaultValue.Request))
    ]

defaultLogFormatter :: LogFormatter
defaultLogFormatter = \display content -> "Not recognized message"
