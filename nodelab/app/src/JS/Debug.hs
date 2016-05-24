module JS.Debug where

import Utils.PreludePlus
import Data.JSString      (pack)
import GHCJS.Types (JSVal, JSString)
import GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import GHCJS.DOM.Types  (UIEvent, Window)
import Data.JSString.Text  (lazyTextToJSString)
import Data.Aeson (ToJSON, toJSON)
import GHCJS.Marshal (toJSVal)


getState :: EventName Window UIEvent
getState = (unsafeEventName (pack "getState"))

foreign import javascript safe "window.state  = $1"         saveState :: JSVal -> IO ()
foreign import javascript safe "window.lastEv = $1"         lastEv    :: JSVal -> IO ()
foreign import javascript safe "console.log($1)"            clog      :: JSVal -> IO ()
foreign import javascript safe "console.info('Incoming message', $1)" cinfo     :: JSVal -> IO ()
foreign import javascript safe "console.error($1, $2)"      error'    :: JSString -> JSVal -> IO ()
foreign import javascript safe "config.exportState" shouldExportState :: Bool
foreign import javascript safe "window.processedEvents.push($1)" processedEvent :: Int -> IO ()

error :: (ToJSON o) => Text -> o -> IO ()
error msg o = do
    let msg' = lazyTextToJSString msg
    o' <- toJSVal $ toJSON o
    error' msg' o'
