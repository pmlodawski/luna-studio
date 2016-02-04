module JS.Debug where

import Utils.PreludePlus
import Data.JSString      (pack)
import GHCJS.Types (JSVal)
import GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import GHCJS.DOM.Types  (UIEvent, Window)

getState :: EventName Window UIEvent
getState = (unsafeEventName (pack "getState"))

foreign import javascript safe "window.state  = $1"         saveState :: JSVal -> IO ()
foreign import javascript safe "window.lastEv = $1"         lastEv    :: JSVal -> IO ()
foreign import javascript safe "console.log($1)"            clog      :: JSVal -> IO ()
foreign import javascript safe "config.exportState" shouldExportState :: Bool
foreign import javascript safe "window.processedEvents.push($1)" processedEvent :: Int -> IO ()
