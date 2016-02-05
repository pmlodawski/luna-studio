{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.WSConnector.Data.WSFrame where

import qualified Data.Binary                        as Binary
import           Data.ByteString                    (ByteString)
import           Data.ByteString.Base64.Lazy        (decodeLenient, encode)
import           Data.ByteString.Lazy               (fromStrict, toStrict)
import           Flowbox.Prelude

import           Flowbox.WSConnector.Data.WSMessage

newtype WSFrame = WSFrame { _messages :: [WSMessage]
                          } deriving (Show, Generic)

makeLenses ''WSFrame
instance Binary.Binary WSFrame

deserializeFrame :: ByteString -> WSFrame
deserializeFrame = Binary.decode . decodeLenient . fromStrict

serializeFrame :: WSFrame -> ByteString
serializeFrame = toStrict . encode . Binary.encode

