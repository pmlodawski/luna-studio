{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Empire.Server where

import           Control.Monad                          (forever)
import           Control.Monad.State                    (StateT, evalStateT, get, put)
import qualified Data.Binary                            as Bin
import           Data.ByteString                        (ByteString)
import           Data.ByteString.Char8                  (unpack)
import           Data.ByteString.Lazy                   (fromStrict, toStrict)
import           Data.Map.Strict                        (Map)
import qualified Data.Map.Strict                        as Map
import qualified Data.Maybe                             as Maybe
import qualified Data.Time.Clock                        as Clock

import qualified Flowbox.Bus.Bus                        as Bus
import           Flowbox.Bus.BusT                       (BusT (..))
import qualified Flowbox.Bus.BusT                       as Bus
import qualified Flowbox.Bus.Data.Message               as Message
import           Flowbox.Bus.Data.MessageFrame          (MessageFrame (MessageFrame))
import           Flowbox.Bus.Data.Topic                 (Topic)
import           Flowbox.Bus.EndPoint                   (BusEndPoints)
import           Empire.Env                             (Env)
import qualified Empire.Env                             as Env
import           Flowbox.Data.Convert
import           Flowbox.Prelude                        hiding (error)
import           Flowbox.System.Log.Logger
import qualified Reexport.Flowbox.Bus.Data.Exception    as Exception
import qualified Empire.Utils                           as Utils
import qualified Empire.Handlers                        as Handlers


logger :: LoggerIO
logger = getLoggerIO $moduleName

run :: BusEndPoints -> [Topic] -> IO (Either Bus.Error ())
run endPoints topics = Bus.runBus endPoints $ do
    logger info $ "Subscribing to topics: " ++ show topics
    logger info $ show endPoints
    mapM_ Bus.subscribe topics
    Bus.runBusT $ evalStateT (forever handleMessage) def

handleMessage :: StateT Env BusT ()
handleMessage = do
    msgFrame <- lift $ BusT Bus.receive'
    case msgFrame of
        Left err -> logger error $ "Unparseable message: " ++ err
        Right (MessageFrame msg crlID senderID lastFrame) -> do
            let topic = msg ^. Message.topic
                logMsg =  show senderID
                       ++ " -> "
                       ++ " (last = "
                       ++ show lastFrame
                       ++ ")"
                       ++ "\t:: "
                       ++ topic
                content = msg ^. Message.message
                errorMsg = show content
            case Utils.lastPart '.' topic of
                "update"   -> handleUpdate  logMsg topic content
                "request"  -> handleRequest logMsg topic content
                _          -> do logger error logMsg
                                 logger error errorMsg

handleRequest :: String -> String -> ByteString -> StateT Env BusT ()
handleRequest logMsg topic content = do
    logger info logMsg
    let handler = Map.findWithDefault defaultHandler topic Handlers.handlersMap
    handler content

defaultHandler :: ByteString -> StateT Env BusT ()
defaultHandler content = do
    logger info $ "Not recognized request"
    logger info $ unpack content

handleUpdate :: String -> String -> ByteString -> StateT Env BusT ()
handleUpdate logMsg topic content = do
    logger info logMsg
    -- logger info $ unpack content
