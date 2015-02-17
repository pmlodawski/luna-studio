---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Flowbox.UR.Manager.Manager where

--import           Flowbox.Bus.Bus        as Bus
--import           Flowbox.Bus.Data.Topic as Topic
--import           Flowbox.Bus.EndPoint   (BusEndPoints)

--run :: BusEndPoints -> [Topics] -> Int
--run ep topics = Bus.runBus ep $ do
--    mapM_ Bus.subscribe topics
--    Bus.runBusT $ evalStateT (forever waitForAction) def

import           Control.Monad                    (forever)
import           Control.Monad.Trans.State.Strict
import           Data.List                        (isSuffixOf)

import qualified Flowbox.Bus.Bus               as Bus
import           Flowbox.Bus.BusT              (BusT)
import qualified Flowbox.Bus.BusT              as Bus
import qualified Flowbox.Bus.Data.Exception    as Exception
import qualified Flowbox.Bus.Data.Message      as Message
import           Flowbox.Bus.Data.MessageFrame (MessageFrame (MessageFrame))
import           Flowbox.Bus.Data.Topic        (Topic)
import qualified Flowbox.Bus.Data.Topic        as Topic
import           Flowbox.Bus.EndPoint          (BusEndPoints)
import           Flowbox.UR.Manager.Env        (Env)
import qualified Flowbox.UR.Manager.Env        as Env
import           Flowbox.Data.Convert
import           Flowbox.Prelude               hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers  as Proto
import qualified Generated.Proto.Bus.Exception as Gen

logger :: LoggerIO
logger = getLoggerIO "asd"

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
            let topic  = msg ^. Message.topic
                content = msg ^. Message.message
            undo <- get
            let (m, a) = undo ^. Env.times
            if "urm.register" `isSuffixOf` topic 
                then do --put $ Env.Env $ (m, (crlID, content) : a)
                        logger info $ "zaladowalem " ++ (show content)
                else if "urm.undo" `isSuffixOf` topic
                    then do
                        case a of
                            []                 -> logger info $ "W miejscu ostatniej zmiany"
                            (action : history) -> --put $ Env.Env $ (m, history)
                                                  logger info $ "cofam: " ++ (show action)
                    else logger info $ "niep rzyjalem " ++ topic--logger info $ foldl (\x id -> (show id) ++ "\n" ++  x) [] $ take 10 a
