---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.Bus.Logger.Logger where

import           Control.Monad                    (forever)
import           Control.Monad.Trans.State.Strict
import           Data.List                        (isSuffixOf)
import qualified Data.Map.Strict                  as Map
import qualified Data.Maybe                       as Maybe
import qualified Data.Time.Clock                  as Clock

import qualified Flowbox.Bus.Bus               as Bus
import           Flowbox.Bus.BusT              (BusT)
import qualified Flowbox.Bus.BusT              as Bus
import qualified Flowbox.Bus.Data.Exception    as Exception
import qualified Flowbox.Bus.Data.Message      as Message
import           Flowbox.Bus.Data.MessageFrame (MessageFrame (MessageFrame))
import           Flowbox.Bus.Data.Topic        (Topic)
import qualified Flowbox.Bus.Data.Topic        as Topic
import           Flowbox.Bus.EndPoint          (BusEndPoints)
import           Flowbox.Bus.Logger.Env        (Env)
import qualified Flowbox.Bus.Logger.Env        as Env
import           Flowbox.Control.Error         (liftIO)
import           Flowbox.Data.Convert
import           Flowbox.Prelude               hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers  as Proto
import qualified Generated.Proto.Bus.Exception as Gen



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
            let topic  = msg ^. Message.topic
                logMsg = show senderID
                       ++ " -> "
                       ++ show crlID
                       ++ " (last = "
                       ++ show lastFrame
                       ++ ")"
                       ++ "\t:: "
                       ++ topic
                       ++ Maybe.maybe  "" (\t -> " [" ++ show t ++ "]") time
                content = msg ^. Message.message
                errorMsg = case Proto.messageGet' content of
                    Left err        -> "(cannot parse error message: " ++ err ++ ")"
                    Right exception -> case (decodeP (exception :: Gen.Exception)) ^. Exception.msg of
                        Nothing           -> "(exception without message)"
                        Just exceptionMsg -> exceptionMsg
            if Topic.error `isSuffixOf` topic
                then do logger error logMsg
                        logger error errorMsg
                else logger info  logMsg


measureTime :: Message.CorrelationID -> StateT Env BusT (Maybe Clock.NominalDiffTime)
measureTime !crlID = do
    stop  <- liftIO Clock.getCurrentTime
    times <- get
    put $! Env.times %~ Map.insert crlID stop $! times
    return $ fmap (Clock.diffUTCTime stop) $ Map.lookup crlID $ times ^. Env.times
