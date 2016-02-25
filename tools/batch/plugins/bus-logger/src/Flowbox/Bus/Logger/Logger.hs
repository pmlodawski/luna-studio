---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module Flowbox.Bus.Logger.Logger where

import           Control.Monad         (forever)
import           Control.Monad.State   (StateT, evalStateT, get, put)
import qualified Data.Binary           as Bin
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.ByteString.Lazy  (fromStrict, toStrict)
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
import           Flowbox.Bus.Logger.Env                 (Env)
import qualified Flowbox.Bus.Logger.Env                 as Env
import           Flowbox.Bus.RPC.Types                  (Response (Response), Result (ErrorResult, Status), Value (Value))
import           Control.Monad.Error                    (MonadError)
import           Flowbox.Data.Convert
import           Flowbox.Prelude                        hiding (error)
import           Flowbox.System.Log.Logger
import qualified Reexport.Flowbox.Bus.Data.Exception    as Exception
import qualified Reexport.G.Proto.Bus.Exception         as Gen



logger :: LoggerIO
logger = getLoggerIO $moduleName


run :: BusEndPoints -> [Topic] -> IO (Either Bus.Error ())
run ep topics = Bus.runBus ep $ do
    logger info $ "Subscribing to topics: " ++ show topics
    mapM_ Bus.subscribe topics
    Bus.runBusT $ evalStateT (forever logMessage) def

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
            case lastPart '.' topic of
                "response" -> do logger info  logMsg
                                 lift $ BusT $ lift $ ppr content
                "status"   -> logger info  logMsg
                "update"   -> logger info  logMsg
                "request"  -> logger info  logMsg
                _          -> do logger error logMsg
                                 logger error errorMsg


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


lastPart :: Eq a => a -> [a] -> [a]
lastPart = lastPartIntern []

lastPartIntern :: Eq a => [a] -> a -> [a] -> [a]
lastPartIntern _      b (a:as) | a == b = lastPartIntern [] b as
lastPartIntern buffer _ []              = reverse buffer
lastPartIntern buffer b (a:as)          = lastPartIntern (a:buffer) b as

measureTime :: MonadIO m => Message.CorrelationID -> StateT Env m (Maybe Clock.NominalDiffTime)
measureTime !crlID = do
    stop  <- liftIO Clock.getCurrentTime
    times <- get
    put $! Env.times %~ Map.insert crlID stop $! times
    return $ fmap (Clock.diffUTCTime stop) $ Map.lookup crlID $ times ^. Env.times
