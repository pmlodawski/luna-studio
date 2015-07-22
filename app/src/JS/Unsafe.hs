module JS.Unsafe where

import           Utils.PreludePlus
import           Utils.Vector

import           GHCJS.Foreign
import           GHCJS.DOM.EventM
import           GHCJS.Types      ( JSRef, JSString )
import           GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import           Data.JSString.Text ( lazyTextFromJSString, lazyTextToJSString )


import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

import           System.IO.Unsafe (unsafePerformIO)


foreign import javascript unsafe "breadcrumb.calculateTextWidth($1)"
    js_calculateTextWidth :: JSString -> IO (Double)

calculateTextWidth :: Text -> Double
calculateTextWidth t = unsafePerformIO $ js_calculateTextWidth $ lazyTextToJSString t


