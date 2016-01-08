{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Empire.Server where

import           Control.Monad         (forever)
import           Control.Monad.State   (StateT, evalStateT, get, put)
import qualified Data.Binary           as Bin
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.ByteString.Lazy  (fromStrict, toStrict)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import qualified Data.Maybe            as Maybe
import qualified Data.Time.Clock       as Clock

import qualified Flowbox.Bus.Bus                        as Bus
import           Flowbox.Bus.BusT                       (BusT (..))
import qualified Flowbox.Bus.BusT                       as Bus
import qualified Flowbox.Bus.Data.Message               as Message
import           Flowbox.Bus.Data.MessageFrame          (MessageFrame (MessageFrame))
import           Flowbox.Bus.Data.Topic                 (Topic)
import           Flowbox.Bus.EndPoint                   (BusEndPoints)
import           Empire.Env                             (Env)
import qualified Empire.Env                             as Env
import           Flowbox.Bus.RPC.Types                  (Response (Response), Result (ErrorResult, Status), Value (Value))
import           Flowbox.Control.Monad.Error            (MonadError)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                        hiding (error)
import           Flowbox.System.Log.Logger
import qualified Reexport.Flowbox.Bus.Data.Exception    as Exception
import           Empire.Utils                           as Utils
import           Empire.API.Topic                       as Topic

logger :: LoggerIO
logger = getLoggerIO $moduleName


run :: BusEndPoints -> [Topic] -> IO (Either Bus.Error ())
run ep topics = Bus.runBus ep $ do
    logger info $ "Subscribing to topics: " ++ show topics
    mapM_ Bus.subscribe topics
    Bus.runBusT $ evalStateT (forever handleMessage) def

logMessage :: StateT Env BusT ()
logMessage = do
    msgFrame <- lift $ Bus.BusT Bus.receive'
    case msgFrame of
        Left err -> logger error $ "Unparseable message: " ++ err
        Right (MessageFrame msg crlID senderID lastFrame) -> do
            time <- measureTime crlID
            let topic = msg ^. Message.topic
                logMsg =  show senderID
                       ++ " -> "
                       ++ " (last = "
                       ++ show lastFrame
                       ++ ")"
                       ++ "\t:: "
                       ++ topic
                       ++ Maybe.maybe  "" (\t -> " [" ++ show t ++ "]") time
                content = msg ^. Message.message
                errorMsg = show content
            case Utils.lastPart '.' topic of
                "response" -> do logger info  logMsg
                                 lift $ BusT $ lift $ ppr content
                "status"   -> logger info  logMsg
                "update"   -> logger info  logMsg
                "request"  -> logger info  logMsg
                _          -> do logger error logMsg
                                 logger error errorMsg

handleMessage :: StateT Env BusT ()
handleMessage = do
    msgFrame <- lift $ Bus.BusT Bus.receive'
    case msgFrame of
        Left err -> logger error $ "Unparseable message: " ++ err
        Right (MessageFrame msg crlID senderID lastFrame) -> do
            time <- measureTime crlID
            let topic = msg ^. Message.topic
                logMsg =  show senderID
                       ++ " -> "
                       ++ " (last = "
                       ++ show lastFrame
                       ++ ")"
                       ++ "\t:: "
                       ++ topic
                       ++ Maybe.maybe  "" (\t -> " [" ++ show t ++ "]") time
                content = msg ^. Message.message
                errorMsg = show content
            case Utils.lastPart '.' topic of
                "update"   -> handleUpdate  logMsg topic content
                "request"  -> handleRequest logMsg topic content
                _          -> do logger error logMsg
                                 logger error errorMsg


type Handler = ByteString -> StateT Env BusT ()

handlersMap :: Map String Handler
handlersMap = Map.fromList
    [ (Topic.addNodeRequest,    handleAddNode)
    , (Topic.removeNodeRequest, handleRemoveNode)
    ]

handleRequest :: String -> String -> ByteString -> StateT Env BusT ()
handleRequest logMsg topic content = do
    logger info logMsg
    let handler = Map.findWithDefault defaultHandler topic handlersMap
    handler content
    -- logger info $ unpack content

defaultHandler :: ByteString -> StateT Env BusT ()
defaultHandler content = do
    logger info $ "Handling not recognized request"
    logger info $ unpack content

handleAddNode :: ByteString -> StateT Env BusT ()
handleAddNode content = do
    logger info $ "Handling AddNodeRequest"
    logger info $ unpack content

handleRemoveNode :: ByteString -> StateT Env BusT ()
handleRemoveNode content = do
    logger info $ "Handling RemoveNodeRequest"
    logger info $ unpack content

handleUpdate :: String -> String -> ByteString -> StateT Env BusT ()
handleUpdate logMsg topic content = do
    logger info logMsg
    logger info $ unpack content

ppr :: (MonadIO m, MonadError String m)
    => ByteString -> m ()
ppr msg = do
    --Request fname (Value typeName protocol dataBytes) <- hoistEither $ RPC.messageGet' msg
    let Response fname result _ = Bin.decode $ fromStrict msg
    case result of
        (Status (Value tname _ dataBytes)) -> do
            logger info  $ unlines [ "requested method: " ++ fname
                                   , "    content type: " ++ tname
                                   ]
            logger debug $ unlines [ "            data: "
                                   , unpack $ toStrict dataBytes
                                   ]
        (ErrorResult err) ->
            logger error $ unlines [ "requested method: " ++ fname
                                   , "  error response: " ++ err
                                   ]

measureTime :: MonadIO m => Message.CorrelationID -> StateT Env m (Maybe Clock.NominalDiffTime)
measureTime !crlID = do
    stop  <- liftIO Clock.getCurrentTime
    times <- get
    put $! Env.times %~ Map.insert crlID stop $! times
    return $ fmap (Clock.diffUTCTime stop) $ Map.lookup crlID $ times ^. Env.times
