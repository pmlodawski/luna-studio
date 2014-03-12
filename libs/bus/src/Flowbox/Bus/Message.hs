---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Bus.Message where

import           Data.ByteString              (ByteString)
import qualified Flowbox.Text.ProtocolBuffers as Proto

import Flowbox.Bus.Topic.Topic (Topic)
import Flowbox.Prelude



type ID = Proto.Int32

type MessageID = ID
type ClientID  = ID


data CorrelationID = CorrelationID { clientID  :: ClientID
                                   , messageID :: MessageID
                                   } deriving (Read, Show, Eq)


data Message = Message { topic   :: Topic
                       , message :: ByteString
                       } deriving (Read, Show, Eq)
