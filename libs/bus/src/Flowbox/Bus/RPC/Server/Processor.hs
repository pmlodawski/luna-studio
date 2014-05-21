---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Flowbox.Bus.RPC.Server.Processor where

import qualified Data.List         as List
import qualified Data.String.Utils as Utils

import           Flowbox.Bus.Data.Exception                     (Exception (Exception))
import           Flowbox.Bus.Data.Message                       (Message (Message))
import qualified Flowbox.Bus.Data.Message                       as Message
import           Flowbox.Bus.Data.Topic                         (Topic)
import           Flowbox.Bus.RPC.Handler                        (BusRPCHandler)
import           Flowbox.Control.Error                          hiding (err)
import           Flowbox.Prelude                                hiding (error)
import           Flowbox.System.Log.Logger
import qualified Flowbox.Text.ProtocolBuffers                   as Proto
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Bus.RPC.Server.Processor"


status :: String
status = "status"


update :: String
update = "update"


singleResult :: (a -> IO b) -> a -> IO [b]
singleResult f a = mkList <$> f a


process :: BusRPCHandler -> Message -> IO [Message]
process handler msg = handler call topic where
    call type_ method = case Proto.messageGet' $ Message.message msg of
        Left err   -> do logger error err
                         return $ respondError topic err
        Right args -> do results <- runEitherT $ scriptIO $ method args
                         return $ case results of
                            Left err -> respondError topic $ "Unhandled error: " ++ err
                            Right ok -> map (respond type_) ok

    topic = Message.topic msg

    respond :: Proto.Serializable msg => String -> msg -> Message
    respond = constructMessage topic


respondError :: Topic -> String -> [Message]
respondError topic = mkList . constructMessage topic "error" . encodeP . Exception . Just


constructMessage :: Proto.Serializable msg => Topic -> String ->  msg -> Message
constructMessage topic type_  data_ = Message newTopic $ Proto.messagePut' data_ where
    newTopic = (List.intercalate "." . flip (++) [type_] . init . Utils.split ".") topic

