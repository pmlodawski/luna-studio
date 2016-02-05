---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Flowbox.Bus.RPC.Client where

import           Data.Binary                   (Binary, decode, encode)
import           Data.ByteString.Lazy          (fromStrict, toStrict)

import           Flowbox.Bus.Bus               (Bus)
import qualified Flowbox.Bus.Bus               as Bus
import qualified Flowbox.Bus.Data.Flag         as Flag
import           Flowbox.Bus.Data.Message      (Message (Message))
import qualified Flowbox.Bus.Data.Message      as Message
import           Flowbox.Bus.Data.MessageFrame (MessageFrame)
import qualified Flowbox.Bus.Data.MessageFrame as MessageFrame
import           Flowbox.Bus.Data.Topic        (Topic)
import qualified Flowbox.Bus.Data.Topic        as Topic
import           Flowbox.Bus.RPC.Types
import qualified Flowbox.Control.Monad.Loops   as Loops
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO $moduleName


isCorrelationIDValid :: Message.CorrelationID -> MessageFrame -> Bool
isCorrelationIDValid correlationID frame =
    frame ^. MessageFrame.correlation == correlationID


isRequest :: MessageFrame -> Bool
isRequest frame =
    Topic.isRequest $ frame ^. MessageFrame.message . Message.topic


allFramesReceived :: Message.CorrelationID -> MessageFrame -> Bool
allFramesReceived correlationID frame =
    not (isRequest frame)
    && isCorrelationIDValid correlationID frame
    && frame ^. MessageFrame.lastFrame == Flag.Enable


query :: (Binary args, Typeable args, Binary result, Typeable result)
      => String -> Topic -> args -> Bus [result]
query pluginName topic args = do
    results <- queryRaw $ Message (pluginName <> "." <> topic) $ toStrict . encode $ Request topic $ packValue args
    mapM (lift . unpackValue . retVal . result . decode . fromStrict .view Message.message) results


queryRaw :: Message -> Bus [Message]
queryRaw message = do
    let topicBase = Topic.base $ message ^. Message.topic
    logger debug "Query : sending..."
    Bus.subscribe topicBase
    correlationID <- Bus.send Flag.Enable message
    logger debug "Query : receiving responses..."
    frames <- Loops.repeatUntil Bus.receive (not . allFramesReceived correlationID)
    Bus.unsubscribe topicBase
    logger debug "Query : complete"
    return $ map (view MessageFrame.message)
           $ filter (not . isRequest)
           $ filter (isCorrelationIDValid correlationID) frames
