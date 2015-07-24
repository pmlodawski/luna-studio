module ThreeJS.Text where

import           Utils.PreludePlus
import           GHCJS.Foreign
import           GHCJS.Types      ( JSRef, JSString )
import           ThreeJS.Types

import           Data.JSString.Text ( lazyTextFromJSString, lazyTextToJSString )
import qualified Data.JSString as JSString

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

data TextGeometry

instance Geometry TextGeometry


foreign import javascript unsafe "require('bmfont').render({text: $1, font: require('font/LatoBlack-sdf')})"
    js_buildTextGeometry :: JSString -> IO (JSRef TextGeometry)


buildTextGeometry :: Text -> IO (JSRef TextGeometry)
buildTextGeometry = js_buildTextGeometry . lazyTextToJSString

data TextMaterial

instance Material TextMaterial

foreign import javascript unsafe "require('font/text_material').graph"
    getTextMaterial :: IO (JSRef TextMaterial)


foreign import javascript unsafe "breadcrumb.calculateTextWidth($1)"
    js_calculateTextWidth :: JSString -> Double

calculateTextWidth :: Text -> Double
calculateTextWidth = js_calculateTextWidth . lazyTextToJSString

