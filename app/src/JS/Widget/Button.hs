module JS.Widget.Button where

import           Utils.PreludePlus
import           Utils.Vector

import           GHCJS.Foreign
import           GHCJS.DOM.EventM
import           GHCJS.Types      ( JSRef, JSString )
import           GHCJS.DOM.EventTargetClosures (EventName, unsafeEventName)
import           Data.JSString.Text ( lazyTextFromJSString, lazyTextToJSString )
import qualified Data.JSString as JSString

import           JavaScript.Array ( JSArray )
import qualified JavaScript.Array as JSArray

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

import qualified JS.Unsafe


data Button

foreign import javascript unsafe "new Button($1, $2, new THREE.Vector2($3, $4), new THREE.Vector2($5, $6))"
    createButtonJS :: JSString -> Int -> Double -> Double -> Double -> Double -> IO (JSRef Button)

createButton :: Text -> Int -> Vector2 Double -> Vector2 Double -> IO (JSRef Button)
createButton label state pos size = createButtonJS  (lazyTextToJSString label) state (pos ^. x) (pos ^. y) (size ^. x) (size ^. y)

foreign import javascript unsafe "breadcrumb.addButton($1)"
    addBreadcrumb :: JSRef Button -> IO ()

foreign import javascript unsafe "breadcrumb.clear()"
    clearBreadcrumb :: IO ()

foreign import javascript unsafe "breadcrumb.setButtonState($1, $2)"
    setButtonState :: Int -> Int -> IO ()

calculateTextWidth = JS.Unsafe.calculateTextWidth