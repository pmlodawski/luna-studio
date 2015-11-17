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

import           Flowbox.Bus.Data.Topic        (Topic, (/+))
import qualified Flowbox.Bus.Data.Topic        as Topic
import           Flowbox.Prelude
import qualified Flowbox.Bus.RPC.RPC           as RPC
import           Flowbox.Bus.RPC.Types
import qualified Flowbox.Text.ProtocolBuffers  as Proto




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


mk :: Topic -> Response -> Message
mk topic' data_ = Message topic' $ RPC.messagePut' data_ where


mkError :: Topic -> FunctionName -> String -> [Message]
mkError topic' functionName = return . mk (topic' /+ Topic.error) . (response :: String -> Response)
	where
		response description = Response functionName (ErrorResult description) []
