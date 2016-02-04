module JS.TextEditor where

import Utils.PreludePlus
import GHCJS.Types
import Data.JSString.Text
import qualified Data.JSString as JSString
import GHCJS.Types
import GHCJS.Foreign.Callback
import GHCJS.Foreign
import JavaScript.Array
import Unsafe.Coerce



foreign import javascript safe "textEditor.setText($1)" setText' :: JSString -> IO ()

setText :: Text -> IO ()
setText = setText' . lazyTextToJSString

foreign import javascript safe "textEditor.callback = $1"
    registerCallback' :: Callback (JSRef () -> IO ()) -> IO ()

foreign import javascript safe "textEditor.callback = function(){ return null; }"
    unregisterCallback' :: Callback (JSRef () -> IO ()) -> IO ()

foreign import javascript safe "$r = $1" toJSString :: JSRef () -> JSString

registerCallback :: (JSRef () -> IO ()) -> IO (IO ())
registerCallback callback = do
    wrappedCallback <- asyncCallback1 callback
    registerCallback' wrappedCallback
    return $ unregisterCallback' wrappedCallback >> releaseCallback wrappedCallback



