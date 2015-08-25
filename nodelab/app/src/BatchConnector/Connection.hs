module BatchConnector.Connection where

import qualified Data.Binary                 as Binary
import           GHC.Generics                (Generic)
import           GHCJS.Types (JSString)
import           Data.JSString.Text
import           Data.Text (Text)
import           Data.ByteString.Lazy.Char8  (ByteString, pack, toStrict)
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.Text.Lazy.Encoding     (decodeUtf8)
import           Utils.PreludePlus           hiding (Text)

data WebMessage = WebMessage { _topic   :: String
                             , _message :: ByteString
                             } deriving (Show, Generic)

instance Binary.Binary WebMessage

serialize :: WebMessage -> JSString
serialize = lazyTextToJSString . decodeUtf8 . B64.encode . Binary.encode

deserialize :: String -> WebMessage
deserialize = Binary.decode . B64.decodeLenient . pack
