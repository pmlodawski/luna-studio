module UI.Widget.Slider where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           GHCJS.Types      ( JSVal, JSString )
import           Data.JSString.Text ( lazyTextFromJSString, lazyTextToJSString )
import qualified Data.JSString as JSString

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import qualified Event.Mouse    as Mouse
import           Event.Keyboard (KeyMods(..))
import           Utils.Vector
import qualified Object.Widget.Slider.Continuous as Model
import           Object.Widget
import           Utils.CtxDynamic
import           Object.UITypes
import           GHCJS.Marshal.Pure(PToJSVal(..), PFromJSVal(..))
import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global as Global
import           Reactive.State.Global (inRegistry)
import qualified Reactive.State.UIRegistry as UIRegistry

import           UI.Widget (UIWidget(..))
import qualified UI.Widget as Widget
import qualified UI.Registry as UI
import qualified UI.Generic  as UI
import           Reactive.Commands.Command (Command, performIO)
import qualified Data.HMap.Lazy as HMap
import           Data.HMap.Lazy (TypeKey(..))

newtype Slider = Slider { unSlider :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget Slider

foreign import javascript unsafe "new Slider($1, $2, $3)"   create'           :: Int    -> Double -> Double -> IO Slider
foreign import javascript unsafe "$1.setValue($2)"          setValue'         :: Slider -> Double           -> IO ()
foreign import javascript unsafe "$1.setLabel($2)"          setLabel'         :: Slider -> JSString         -> IO ()
foreign import javascript unsafe "$1.setValueLabel($2)"     setValueLabel'    :: Slider -> JSString         -> IO ()
foreign import javascript unsafe "$1.setFocus($2)"          setFocus'         :: Slider -> Bool             -> IO ()
