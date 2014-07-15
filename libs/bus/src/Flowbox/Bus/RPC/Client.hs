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

module Flowbox.Bus.RPC.Client where

import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Either

import           Flowbox.Bus.Bus               (Bus)
import qualified Flowbox.Bus.Bus               as Bus
import qualified Flowbox.Bus.Data.Flag         as Flag
import           Flowbox.Bus.Data.Message      (Message (Message))
import qualified Flowbox.Bus.Data.Message      as Message
import           Flowbox.Bus.Data.MessageFrame (MessageFrame)
import qualified Flowbox.Bus.Data.MessageFrame as MessageFrame
import           Flowbox.Bus.Data.Topic        (Topic)
import qualified Flowbox.Bus.Data.Topic        as Topic
import qualified Flowbox.Control.Monad.Loops   as Loops
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers  as Proto



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.RPC.Client"


isCorrelationIDValid :: Message.CorrelationID -> MessageFrame -> Bool
isCorrelationIDValid correlationID frame =
    frame ^. MessageFrame.correlation == correlationID


isRequest :: MessageFrame -> Bool
isRequest frame =
    Topic.isRequest $ frame ^. MessageFrame.message . Message.topic


allFramesReceived :: Message.CorrelationID -> MessageFrame -> Bool
allFramesReceived correlationID frame =
    (not $ isRequest frame)
    && isCorrelationIDValid correlationID frame
    && frame ^. MessageFrame.lastFrame == Flag.Enable


query :: (Proto.Serializable args, Proto.Serializable result)
      => Topic -> args -> Bus [result]
query topic args = do
    results <- query_raw $ Message topic $ Proto.messagePut' args
    mapM (lift . hoistEither . Proto.messageGet' . view Message.message) results


query_raw :: Message -> Bus [Message]
query_raw message = do
    let topicBase = Topic.base $ message ^. Message.topic
    Bus.subscribe topicBase
    logger debug "Query : sending..."
    correlationID <- Bus.send Flag.Enable message
    logger debug "Query : receiving responses..."
    frames <- Loops.repeatUntil Bus.receive (not . allFramesReceived correlationID)
    Bus.unsubscribe topicBase
    logger debug "Query : complete"
    return $ map (view MessageFrame.message)
           $ filter (not . isRequest)
           $ filter (isCorrelationIDValid correlationID) frames

