{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Generic where

import           Utils.PreludePlus

import           GHCJS.Foreign
import           Utils.Vector
import           GHCJS.Types         (JSVal)
import           Object.Widget hiding (setPosition)
import qualified Data.JSString as JSString
import           Object.UITypes
import           GHCJS.Marshal.Pure(PToJSVal(..), PFromJSVal(..))

import           Reactive.Commands.Command (Command, ioCommand, performIO)
import qualified Reactive.State.UIRegistry as UIRegistry
import           UI.Widget (UIWidget, UIContainer)
import qualified UI.Registry as UIR

newtype GenericWidget = GenericWidget { unGenericWidget :: JSVal } deriving (PToJSVal, PFromJSVal)

instance UIWidget    GenericWidget
instance UIContainer GenericWidget


foreign import javascript unsafe "$1.mesh.position.x = $2; $1.mesh.position.y = $3"
    setWidgetPosition'      :: JSVal -> Double -> Double -> IO ()

setWidgetPosition :: UIWidget a => Vector2 Double -> a -> IO ()
setWidgetPosition (Vector2 x y) widget = setWidgetPosition' (pToJSVal widget) x y

updatePosition :: (IsDisplayObject b) => WidgetFile a b -> Command (UIRegistry.State a) ()
updatePosition file = performIO $ do
    let position = file ^. widget . widgetPosition
    w <- UIR.lookup $ file ^. objectId :: IO (GenericWidget)
    setWidgetPosition position w

updatePosition' :: WidgetId -> Vector2 Double -> IO ()
updatePosition' id pos = do
        w <- UIR.lookup $ id :: IO (GenericWidget)
        setWidgetPosition pos w
