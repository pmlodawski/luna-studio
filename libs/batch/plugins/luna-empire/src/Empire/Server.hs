{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Empire.Server where

import           Control.Concurrent.STM            (STM)
import           Control.Concurrent.STM.TChan      (TChan, newTChan, readTChan, tryPeekTChan)
import           Control.Concurrent                (forkIO)
import           Control.Monad                     (forever)
import           Control.Monad.State               (StateT, evalStateT)
import           Control.Monad.STM                 (atomically)
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Char8             (unpack)
import qualified Data.Map.Strict                   as Map
import qualified Data.Binary                       as Bin
import           Data.ByteString.Lazy              (toStrict)

import           Empire.API.Data.AsyncUpdate       (AsyncUpdate (..))
import qualified Empire.API.Topic                  as Topic
import qualified Empire.API.Graph.NodeUpdate       as NodeUpdate
import qualified Empire.API.Control.EmpireStarted  as EmpireStarted
import           Empire.API.Data.GraphLocation     (GraphLocation)
import           Empire.Data.Graph                 (Graph)

import qualified Empire.Commands.Library           as Library
import qualified Empire.Commands.Project           as Project
import qualified Empire.Commands.Typecheck         as Typecheck
import qualified Empire.Empire                     as Empire
import           Empire.Env                        (Env)
import qualified Empire.Env                        as Env
import qualified Empire.Handlers                   as Handlers
import qualified Empire.Server.Server              as Server
import qualified Empire.Utils                      as Utils
import           Flowbox.Bus.Bus                   (Bus)
import qualified Flowbox.Bus.Bus                   as Bus
import           Flowbox.Bus.BusT                  (BusT (..))
import qualified Flowbox.Bus.BusT                  as BusT
import qualified Flowbox.Bus.Data.Flag             as Flag
import           Flowbox.Bus.Data.Message          (Message)
import qualified Flowbox.Bus.Data.Message          as Message
import           Flowbox.Bus.Data.MessageFrame     (MessageFrame (MessageFrame))
import           Flowbox.Bus.Data.Topic            (Topic)
import           Flowbox.Bus.EndPoint              (BusEndPoints)
import           Flowbox.Prelude
import qualified Flowbox.System.Log.Logger         as Logger


logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)

sendStarted :: BusEndPoints -> IO ()
sendStarted endPoints = do
    let content = toStrict . Bin.encode $ EmpireStarted.Status
    void $ Bus.runBus endPoints $ Bus.send Flag.Enable $ Message.Message Topic.controlEmpireStarted content

run :: BusEndPoints -> [Topic] -> Bool -> IO (Either Bus.Error ())
run endPoints topics formatted = do
    logger Logger.info $ "Subscribing to topics: " <> show topics
    logger Logger.info $ (Utils.display formatted) endPoints
    sendStarted endPoints
    toBusChan      <- atomically newTChan
    fromEmpireChan <- atomically newTChan
    tcChan         <- atomically newTChan
    let env     = Env.make toBusChan fromEmpireChan tcChan
    let commEnv = Empire.CommunicationEnv fromEmpireChan tcChan
    forkIO $ void $ Bus.runBus endPoints $ BusT.runBusT $ evalStateT (startAsyncUpdateWorker fromEmpireChan) env
    forkIO $ void $ Bus.runBus endPoints $ startToBusWorker toBusChan
    forkIO $ void $ Bus.runBus endPoints $ startTCWorker commEnv tcChan
    Bus.runBus endPoints $ do
        mapM_ Bus.subscribe topics
        BusT.runBusT $ evalStateT (runBus formatted) env

runBus :: Bool -> StateT Env BusT ()
runBus formatted = do
    Env.formatted .= formatted
    createDefaultState
    forever handleMessage

readAll :: TChan a -> STM a
readAll chan = do
    v    <- readTChan chan
    next <- tryPeekTChan chan
    case next of
        Nothing -> return v
        Just _  -> readAll chan

startTCWorker :: Empire.CommunicationEnv -> TChan (GraphLocation, Graph) -> Bus ()
startTCWorker env chan = liftIO $ void $ Empire.runEmpire env def $ forever $ do
    (loc, g) <- liftIO $ atomically $ readAll chan
    Empire.graph .= g
    Typecheck.run loc


startToBusWorker :: TChan Message -> Bus ()
startToBusWorker toBusChan = forever $ do
    msg <- liftIO $ atomically $ readTChan toBusChan
    Bus.send Flag.Enable msg

startAsyncUpdateWorker :: TChan AsyncUpdate -> StateT Env BusT ()
startAsyncUpdateWorker asyncChan = forever $ do
    update <- liftIO $ atomically $ readTChan asyncChan
    case update of
        NodeUpdate   up -> Server.sendToBus Topic.nodeUpdate up
        ResultUpdate up -> Server.sendToBus Topic.nodeResultUpdate up

createDefaultState :: StateT Env BusT ()
createDefaultState = do
    let projectName = Just "default project"
        projectPath = "hello.luna"
        libraryName = Just "default library"
        libraryPath = "main.luna"
    currentEmpireEnv <- use Env.empireEnv
    empireNotifEnv   <- use Env.empireNotif
    formatted        <- use Env.formatted
    (resultProject, newEmpireEnv1) <- liftIO $ Empire.runEmpire empireNotifEnv currentEmpireEnv $ Project.createProject
        projectName (fromString projectPath)
    case resultProject of
        Left err -> logger Logger.error $ Server.errorMessage <> err
        Right (projectId, project) -> do
            logger Logger.info $ "Created project " <> show projectId
            logger Logger.debug $ (Utils.display formatted) project
            Env.empireEnv .= newEmpireEnv1
            (resultLibrary, newEmpireEnv2) <- liftIO $ Empire.runEmpire empireNotifEnv newEmpireEnv1 $ Library.createLibrary
                projectId libraryName (fromString libraryPath)
            case resultLibrary of
                Left err -> logger Logger.error $ Server.errorMessage <> err
                Right (libraryId, library) -> do
                    Env.empireEnv .= newEmpireEnv2
                    logger Logger.info $ "Created library " <> show libraryId
                    logger Logger.debug $ (Utils.display formatted) library
                    return ()

handleMessage :: StateT Env BusT ()
handleMessage = do
    msgFrame <- lift $ BusT Bus.receive'
    case msgFrame of
        Left err -> logger Logger.error $ "Unparseable message: " ++ err
        Right (MessageFrame msg crlID senderID lastFrame) -> do
            let topic = msg ^. Message.topic
                logMsg = show (crlID ^. Message.messageID) <> ": " <> show senderID
                         <> " -> (last = " <> show lastFrame
                         <> ")\t:: " <> topic
                content = msg ^. Message.message
            case Utils.lastPart '.' topic of
                "update"  -> handleUpdate        logMsg topic content
                "status"  -> handleStatus        logMsg topic content
                "request" -> handleRequest       logMsg topic content
                "debug"   -> handleDebug         logMsg topic content
                _         -> handleNotRecognized logMsg topic content


defaultHandler :: ByteString -> StateT Env BusT ()
defaultHandler content = do
    logger Logger.error $ "Not recognized request"
    logger Logger.info $ unpack content

handleRequest :: String -> String -> ByteString -> StateT Env BusT ()
handleRequest logMsg topic content = do
    logger Logger.info logMsg
    let handler = Map.findWithDefault defaultHandler topic Handlers.handlersMap
    handler content

handleUpdate :: String -> String -> ByteString -> StateT Env BusT ()
handleUpdate logMsg _ content = do
    logger Logger.info logMsg

handleStatus :: String -> String -> ByteString -> StateT Env BusT ()
handleStatus logMsg _ content = do
    logger Logger.info logMsg

handleDebug :: String -> String -> ByteString -> StateT Env BusT ()
handleDebug logMsg _ content = do
    logger Logger.info logMsg
    currentEmpireEnv <- use Env.empireEnv
    formatted        <- use Env.formatted
    logger Logger.debug $ Utils.display formatted currentEmpireEnv

handleNotRecognized :: String -> String -> ByteString -> StateT Env BusT ()
handleNotRecognized logMsg _ content = do
    logger Logger.error logMsg
    logger Logger.error $ show content
