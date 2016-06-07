module BatchConnector.Connection where

import           Data.Binary (Binary)
import qualified Data.Binary                 as Binary
import           GHCJS.Types (JSString)
import           Data.JSString.Text
import           Data.UUID.Types (UUID)
import           Data.ByteString.Lazy.Char8  (ByteString, pack)
import qualified Data.ByteString.Base64.Lazy as Base64
import           Data.Text.Lazy.Encoding     (decodeUtf8)
import           Utils.PreludePlus           hiding (Text)
import           JS.WebSocket
import qualified Empire.API.Topic                  as Topic
import           Empire.API.Request                (Request(..))

data ControlCode = ConnectionTakeover
                 | Welcome
                 deriving (Eq, Show, Generic)

instance Binary.Binary ControlCode

data WebMessage = WebMessage { _topic   :: String
                             , _message :: ByteString
                             }
                | ControlMessage ControlCode
                deriving (Eq, Show, Generic)

makeLenses ''WebMessage
instance Binary.Binary WebMessage

data Frame = Frame { _messages :: [WebMessage] } deriving (Show, Generic)

makeLenses ''Frame
instance Binary.Binary Frame

serialize :: Frame -> JSString
serialize = lazyTextToJSString . decodeUtf8 . Base64.encode . Binary.encode

deserialize :: String -> Frame
deserialize = Binary.decode . Base64.decodeLenient . pack

sendMessages :: [WebMessage] -> IO ()
sendMessages msgs = do
    socket <- getWebSocket
    send socket $ serialize $ Frame msgs

sendMessage :: WebMessage -> IO ()
sendMessage msg = sendMessages [msg]

makeMessage :: (Topic.MessageTopic (Request a), Binary a) => UUID -> a -> WebMessage
makeMessage uuid body = let body' = Request uuid body in WebMessage (Topic.topic body') (Binary.encode body')

sendRequest :: (Topic.MessageTopic (Request a), Binary a) => UUID -> a -> IO ()
sendRequest = sendMessage .: makeMessage

sendRequests :: (Topic.MessageTopic (Request a), Binary a) => [(UUID, a)] -> IO ()
sendRequests msgs = sendMessages $ uncurry makeMessage <$> msgs

