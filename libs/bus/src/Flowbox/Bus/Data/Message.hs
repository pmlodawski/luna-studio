---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Bus.Data.Message where

import Data.ByteString (ByteString)

import           Flowbox.Bus.Data.Exception                     (Exception (Exception))
import           Flowbox.Bus.Data.Topic                         (Topic)
import qualified Flowbox.Bus.Data.Topic                         as Topic
import           Flowbox.Prelude
import qualified Flowbox.Text.ProtocolBuffers                   as Proto
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic



type ID = Proto.Int32

type RequestID = ID
type ClientID  = ID


data CorrelationID = CorrelationID { _clientID  :: ClientID
                                   , _messageID :: RequestID
                                   } deriving (Read, Show, Eq)


data Message = Message { _topic   :: Topic
                       , _message :: ByteString
                       } deriving (Read, Show, Eq)


makeLenses(''CorrelationID)
makeLenses(''Message)



mkResponse :: Proto.Serializable msg => Topic -> String -> msg -> Message
mkResponse topic' type_  data_ = Message newTopic $ Proto.messagePut' data_ where
    newTopic = Topic.respond topic' type_


mkError :: Topic -> String -> [Message]
mkError topic' = mkList . mkResponse topic' Topic.error . encodeP . Exception . Just

