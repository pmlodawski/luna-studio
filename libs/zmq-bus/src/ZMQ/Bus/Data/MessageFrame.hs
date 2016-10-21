{-# LANGUAGE TemplateHaskell #-}

module ZMQ.Bus.Data.MessageFrame where

import qualified Data.ByteString.Char8   as Char8

import           Flowbox.Data.ByteString (ByteString)
import qualified Flowbox.Data.ByteString as ByteString
import           Flowbox.Prelude
import           ZMQ.Bus.Data.Flag       (Flag)
import           ZMQ.Bus.Data.Message    (Message (Message))
import qualified ZMQ.Bus.Data.Message    as M
import qualified ZMQ.Bus.Data.Topic      as Topic



data MessageFrame = MessageFrame { _message     :: Message
                                 , _correlation :: M.CorrelationID
                                 , _senderID    :: M.ClientID
                                 , _lastFrame   :: Flag
                                 } deriving (Read, Show, Eq)

makeLenses(''MessageFrame)


separator :: Char
separator = ' '


encode :: Show a => a -> ByteString
encode = Char8.pack . show


decode :: Read a => ByteString -> a
decode = read . Char8.unpack


toByteString :: MessageFrame -> ByteString
toByteString (MessageFrame (Message topic message')
                           (M.CorrelationID clientID messageID)
                           senderID'
                           lastFrame'
             ) =
    ByteString.intercalate (Char8.singleton separator)
                           [ Topic.toByteString topic
                               , encode clientID
                               , encode messageID
                           , encode senderID'
                           , encode lastFrame'
                           , message'
                           ]


fromByteString :: ByteString -> Either String MessageFrame
fromByteString bs = case ByteString.splitFirsts 6 separator bs of
    [topic, clientID, messageID, senderID', lastFrame', message']
          -> Right $ MessageFrame (Message (Topic.fromByteString topic) message')
                                  (M.CorrelationID (decode clientID) (decode messageID))
                                  (decode senderID')
                                  (decode lastFrame')
    wrong -> Left $ "Cannot parse message" ++ show wrong
