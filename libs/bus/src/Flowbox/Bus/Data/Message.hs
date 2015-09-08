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

import           Flowbox.Bus.Data.Exception    (Exception (Exception))
import           Flowbox.Bus.Data.Topic        (Topic, (/+))
import qualified Flowbox.Bus.Data.Topic        as Topic
import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Flowbox.Text.ProtocolBuffers  as Proto
import qualified Generated.Proto.Bus.Exception as Gen



type ID = Proto.Int32

type RequestID = ID
type ClientID  = ID


data CorrelationID = CorrelationID { _clientID  :: ClientID
                                   , _messageID :: RequestID
                                   } deriving (Read, Show, Eq, Ord)


data Message = Message { _topic   :: Topic
                       , _message :: ByteString
                       } deriving (Read, Show, Eq)


makeLenses(''CorrelationID)
makeLenses(''Message)


instance Default CorrelationID where
    def = CorrelationID def def


mk :: Proto.Serializable msg => Topic -> msg -> Message
mk topic' data_ = Message topic' $ Proto.messagePut' data_ where


mkError :: Topic -> String -> [Message]
mkError topic' = return . mk (topic' /+ Topic.error) . (encodeP . Exception . Just :: String -> Gen.Exception)
