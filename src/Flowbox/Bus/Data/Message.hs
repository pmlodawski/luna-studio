---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Bus.Data.Message where

import           Data.ByteString              (ByteString)
import qualified Flowbox.Text.ProtocolBuffers as Proto

import Flowbox.Bus.Data.Topic (Topic)
import Flowbox.Prelude



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
