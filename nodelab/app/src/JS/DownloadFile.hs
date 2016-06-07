module JS.DownloadFile (downloadFile) where

import qualified Data.JSString     as JSString
import           GHCJS.Foreign
import           GHCJS.Types       (JSString)
import           Utils.PreludePlus
import           Utils.Vector
import           Data.Text.Lazy              (Text, pack)
import           Data.JSString.Text  (lazyTextToJSString, lazyTextFromJSString)
import           GHCJS.Nullable            (Nullable, maybeToNullable)

foreign import javascript unsafe "app.downloadFile($2, $1)" downloadFile' :: JSString -> JSString -> IO ()

downloadFile :: Text -> Text -> IO ()
downloadFile name payload = downloadFile' (lazyTextToJSString name) (lazyTextToJSString payload)

