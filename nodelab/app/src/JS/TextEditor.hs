module JS.TextEditor where

import Utils.PreludePlus
import GHCJS.Types
import Data.JSString.Text

foreign import javascript unsafe "textEditor.setText($1)" setText' :: JSString -> IO ()

setText :: Text -> IO ()
setText = setText' . lazyTextToJSString
