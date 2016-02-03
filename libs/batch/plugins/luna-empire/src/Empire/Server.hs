{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Empire.Server where

import           Flowbox.Prelude
import           Control.Monad                          (forever)
import           Control.Monad.State                    (StateT, evalStateT)
import           Data.ByteString                        (ByteString)
import           Data.ByteString.Char8                  (unpack)
import qualified Data.Map.Strict                        as Map
import qualified Empire.Env                             as Env
import           Empire.Env                             (Env)
import qualified Flowbox.Bus.Bus                        as Bus
import           Flowbox.Bus.BusT                       (BusT (..))
import qualified Flowbox.Bus.BusT                       as BusT
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


logger :: Logger.LoggerIO
logger = Logger.getLoggerIO $(Logger.moduleName)

run :: BusEndPoints -> [Topic] -> Bool -> IO (Either Bus.Error ())
run endPoints topics formatted = Bus.runBus endPoints $ do
    logger Logger.info $ "Subscribing to topics: " <> show topics
    logger Logger.info $ (Utils.display formatted) endPoints
    mapM_ Bus.subscribe topics
    BusT.runBusT $ evalStateT (runBus formatted) def

runBus :: Bool -> StateT Env BusT ()
runBus formatted = do
    Env.formatted .= formatted
    createDefaultState
    forever handleMessage

createDefaultState :: StateT Env BusT ()
createDefaultState = do
    let projectName = Just "default project"
        projectPath = "hello.luna"
        libraryName = Just "default library"
        libraryPath = "main.luna"
    currentEmpireEnv <- use Env.empireEnv
    formatted        <- use Env.formatted
    (resultProject, newEmpireEnv1) <- liftIO $ Empire.runEmpire currentEmpireEnv $ Project.createProject
        projectName (fromString projectPath)
    case resultProject of
        Left err -> logger Logger.error $ Server.errorMessage <> err
        Right (projectId, project) -> do
            logger Logger.info $ "Created project " <> show projectId
            logger Logger.debug $ (Utils.display formatted) project
            Env.empireEnv .= newEmpireEnv1
            (resultLibrary, newEmpireEnv2) <- liftIO $ Empire.runEmpire newEmpireEnv1 $ Library.createLibrary
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
