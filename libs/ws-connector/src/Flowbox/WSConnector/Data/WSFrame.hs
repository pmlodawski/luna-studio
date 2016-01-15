{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.WSConnector.Data.WSFrame where

import           Flowbox.Prelude
import           Data.ByteString.Lazy        (toStrict, fromStrict)
import           Data.ByteString             (ByteString)
import           Data.ByteString.Base64.Lazy (decodeLenient, encode)
import qualified Data.Binary                 as Binary

import           Flowbox.WSConnector.Data.WSMessage

newtype WSFrame = WSFrame { _messages :: [WSMessage]
                          } deriving (Show, Generic)

makeLenses ''WSFrame
instance Binary.Binary WSFrame

deserializeFrame :: ByteString -> WSFrame
deserializeFrame = Binary.decode . decodeLenient . fromStrict

serializeFrame :: WSFrame -> ByteString
serializeFrame = toStrict . encode . Binary.encode

