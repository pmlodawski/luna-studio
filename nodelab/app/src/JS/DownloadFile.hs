module JS.DownloadFile (downloadFile) where

import           GHCJS.Types       (JSString)
import           Utils.PreludePlus
import           Data.JSString.Text  (lazyTextToJSString)

foreign import javascript safe "require('DownloadFile').downloadFile($2, $1)" downloadFile' :: JSString -> JSString -> IO ()

downloadFile :: Text -> Text -> IO ()
downloadFile name payload = downloadFile' (lazyTextToJSString name) (lazyTextToJSString payload)

