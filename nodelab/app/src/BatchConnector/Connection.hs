module BatchConnector.Connection where

import           Data.Binary (Binary)
import qualified Data.Binary                 as Binary
import           GHC.Generics                (Generic)
import           GHCJS.Types (JSString)
import           Data.JSString.Text
import           Data.Text (Text)
import           Data.ByteString.Lazy.Char8  (ByteString, pack, toStrict)
import qualified Data.ByteString.Base64.Lazy as Base64
import           Data.Text.Lazy.Encoding     (decodeUtf8)
import           Utils.PreludePlus           hiding (Text)
import           JS.WebSocket

data ControlCode = ConnectionTakeover
                 | Welcome
                 deriving (Show, Generic)

instance Binary.Binary ControlCode

data WebMessage = WebMessage { _topic   :: String
                             , _message :: ByteString
                             }
                | ControlMessage ControlCode
                deriving (Show, Generic)

makeLenses ''WebMessage
instance Binary.Binary WebMessage

data Frame = Frame { _messages :: [WebMessage] } deriving (Show, Generic)

makeLenses ''Frame
instance Binary.Binary Frame

serialize :: Frame -> JSString
serialize = lazyTextToJSString . decodeUtf8 . Base64.encode . Binary.encode

deserialize :: String -> Frame
deserialize = Binary.decode . Base64.decodeLenient . pack

sendMessage :: WebMessage -> IO ()
sendMessage msg = sendMany [msg]

sendRequest :: Binary a => String -> a -> IO ()
sendRequest topic body = sendMessage $ WebMessage topic $ Binary.encode body

sendMany :: [WebMessage] -> IO ()
sendMany msgs = do
    socket <- getWebSocket
    send socket $ serialize $ Frame msgs
