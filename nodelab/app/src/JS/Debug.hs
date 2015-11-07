module JS.Debug where

import Utils.PreludePlus
import Data.JSString      (pack)
import GHCJS.Types (JSVal)
import GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import GHCJS.DOM.Types  (UIEvent, Window)

getState :: EventName Window UIEvent
getState = (unsafeEventName (pack "getState"))

foreign import javascript unsafe "window.state = $1"          saveState :: JSVal -> IO ()
foreign import javascript unsafe "console.log($1)"            clog      :: JSVal -> IO ()
foreign import javascript unsafe "config.exportState" shouldExportState :: Bool
