{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.WSConnector.Data.WSMessage where

import Flowbox.Prelude
import Data.Binary
import Data.ByteString (ByteString)

data ControlCode = ConnectionTakeover
                 | Welcome
                 deriving (Show, Generic)

instance Binary ControlCode

data WSMessage = WebMessage { _topic :: String
                            , _message :: ByteString
                            }
               | ControlMessage ControlCode
               deriving (Show, Generic)

makeLenses ''WSMessage
instance Binary WSMessage
