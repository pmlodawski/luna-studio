---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Bus.MessageFrame where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as ByteString
import qualified Data.ByteString.Char8 as Char8

import           Flowbox.Bus.Message (Message (Message))
import qualified Flowbox.Bus.Message as M
import           Flowbox.Prelude



data MessageFrame = MessageFrame { message     :: Message
                                 , correlation :: M.CorrelationID
                                 , senderID    :: M.ClientID
                                 } deriving (Read, Show, Eq)


separator :: Char
separator = ' '


encode :: (Show a, Read a) => a -> ByteString
encode = Char8.pack . show


decode :: (Show a, Read a) => ByteString -> a
decode = read . Char8.unpack


toByteString :: MessageFrame -> ByteString
toByteString (MessageFrame (Message topic message')
                           (M.CorrelationID clientID messageID)
                           senderID'
             ) =
    ByteString.intercalate ( Char8.singleton separator )
                           [ topic
                               , encode clientID
                               , encode messageID
                           , encode senderID'
                           , message'
                           ]


fromByteString :: ByteString -> Either String MessageFrame
fromByteString bs = case Char8.split separator bs of
    [topic, clientID, messageID, senderID', message']
        -> Right $ MessageFrame (Message topic message')
                                (M.CorrelationID (decode clientID) (decode messageID))
                                (decode senderID')
    _   -> Left "Cannot parse message"
