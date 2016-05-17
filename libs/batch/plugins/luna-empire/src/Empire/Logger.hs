{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Empire.Logger where

import           Control.Monad                     (forever)
import           Control.Monad.State               (StateT, evalStateT)
import qualified Data.Binary                       as Bin
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Char8             (unpack)
import           Data.ByteString.Lazy              (fromStrict, toStrict)
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Prelude                           (undefined)
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
import qualified Empire.API.Library.CreateLibrary  as CreateLibrary
import qualified Empire.API.Library.ListLibraries  as ListLibraries
import qualified Empire.API.Project.CreateProject  as CreateProject
import qualified Empire.API.Project.ListProjects   as ListProjects
import qualified Empire.API.Topic                  as Topic
import qualified Empire.Commands.Library           as Library
import qualified Empire.Commands.Project           as Project
import qualified Empire.Empire                     as Empire
import           Empire.Env                        (LoggerEnv)
import qualified Empire.Env                        as Env
import qualified Empire.Handlers                   as Handlers
import qualified Empire.Utils                      as Utils

import qualified Flowbox.Bus.Bus                   as Bus
import           Flowbox.Bus.BusT                  (BusT (..))
import qualified Flowbox.Bus.BusT                  as Bus
import qualified Flowbox.Bus.Data.Message          as Message
import           Flowbox.Bus.Data.MessageFrame     (MessageFrame (MessageFrame))
import           Flowbox.Bus.Data.Topic            (Topic)
import           Flowbox.Bus.EndPoint              (BusEndPoints)
import qualified Flowbox.System.Log.Logger         as Logger


logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)

run :: BusEndPoints -> [Topic] -> Bool -> IO (Either Bus.Error ())
run endPoints topics formatted = Bus.runBus endPoints $ do
    logger Logger.info $ "Subscribing to topics: " <> show topics
    logger Logger.info $ (Utils.display formatted) endPoints
    mapM_ Bus.subscribe topics
    Bus.runBusT $ evalStateT (runBus formatted) def

runBus :: Bool -> StateT LoggerEnv BusT ()
runBus formatted = do
    Env.formatLog .= formatted
    forever handleMessage

handleMessage :: StateT LoggerEnv BusT ()
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
                "update"  -> logMessage logMsg topic content
                "status"  -> logMessage logMsg topic content
                "request" -> logMessage logMsg topic content
                "debug"   -> logMessage logMsg topic content
                _         -> do logger Logger.error logMsg
                                logger Logger.error errorMsg

type LogFormatter = (forall a. Show a => a -> String) -> ByteString -> String

logMessage :: String -> String -> ByteString -> StateT LoggerEnv BusT ()
logMessage logMsg topic content = do
    formatted <- use Env.formatLog
    logger Logger.info logMsg
    let logFormatter = Map.findWithDefault defaultLogFormatter topic loggFormattersMap
    logger Logger.debug $ logFormatter (Utils.display formatted) content


makeHandler :: forall a. (Topic.MessageTopic a, Bin.Binary a, Show a) => a -> (String, LogFormatter)
makeHandler h = (Topic.topic (undefined :: a), process) where
   process display content = display request where request = (Bin.decode . fromStrict $ content :: a)

loggFormattersMap :: Map String LogFormatter
loggFormattersMap = Map.fromList
    [ makeHandler (undefined :: AddNode.Request          )
    , makeHandler (undefined :: AddNode.Response         )
    , makeHandler (undefined :: AddNode.Update           )
    , makeHandler (undefined :: RemoveNode.Request       )
    , makeHandler (undefined :: RemoveNode.Response      )
    , makeHandler (undefined :: RemoveNode.Update        )
    , makeHandler (undefined :: UpdateNodeMeta.Request   )
    , makeHandler (undefined :: UpdateNodeMeta.Response  )
    , makeHandler (undefined :: UpdateNodeMeta.Update    )
    , makeHandler (undefined :: RenameNode.Request       )
    , makeHandler (undefined :: RenameNode.Response      )
    , makeHandler (undefined :: RenameNode.Update        )
    , makeHandler (undefined :: Connect.Request          )
    , makeHandler (undefined :: Connect.Response         )
    , makeHandler (undefined :: Connect.Update           )
    , makeHandler (undefined :: Disconnect.Request       )
    , makeHandler (undefined :: Disconnect.Response      )
    , makeHandler (undefined :: Disconnect.Update        )
    , makeHandler (undefined :: GetProgram.Request       )
    , makeHandler (undefined :: GetProgram.Response      )
    , makeHandler (undefined :: NodeUpdate.Update        )
    , makeHandler (undefined :: NodeResultUpdate.Update  )
    , makeHandler (undefined :: CodeUpdate.Update        )
    , makeHandler (undefined :: CreateProject.Request    )
    , makeHandler (undefined :: CreateProject.Response   )
    , makeHandler (undefined :: CreateProject.Update     )
    , makeHandler (undefined :: ListProjects.Request     )
    , makeHandler (undefined :: ListProjects.Response    )
    , makeHandler (undefined :: CreateLibrary.Request    )
    , makeHandler (undefined :: CreateLibrary.Response   )
    , makeHandler (undefined :: CreateLibrary.Update     )
    , makeHandler (undefined :: ListLibraries.Request    )
    , makeHandler (undefined :: ListLibraries.Response   )
    , makeHandler (undefined :: SetDefaultValue.Request  )
    , makeHandler (undefined :: SetDefaultValue.Response )
    , (Topic.controlEmpireStarted,   \display content -> "Luna empire started")
    , (Topic.logEnvDebug,            \display content -> "Log environment")
    , (Topic.logEnvDebugGraphViz,    \display content -> "Dump graphviz")
    ]

defaultLogFormatter :: LogFormatter
defaultLogFormatter = \display content -> "Not recognized message"
