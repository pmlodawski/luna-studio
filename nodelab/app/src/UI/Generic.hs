{-# LANGUAGE Rank2Types #-}

module UI.Generic where

import           Utils.PreludePlus

import           Utils.Vector

import qualified Data.JSString             as JSString
import           GHCJS.Foreign
import           GHCJS.Marshal.Pure        (PFromJSVal (..), PToJSVal (..))
import           GHCJS.Types               (JSVal)

import qualified Event.Mouse               as Mouse
import           Object.Widget             (DragState (..), IsDisplayObject, WidgetFile, WidgetId, objectId, widget,
                                            widgetPosition, widgetSize)
import           Reactive.Commands.Command (Command, ioCommand, performIO)
import qualified Reactive.State.Camera     as Camera
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.UIRegistry as UIRegistry

import qualified UI.Registry               as UIR
import           UI.Widget                 (GenericWidget (..), UIContainer, UIWidget)

foreign import javascript unsafe "$1.mesh.position.x = $2; $1.mesh.position.y = $3"
    setWidgetPosition'      :: JSVal -> Double -> Double -> IO ()

foreign import javascript unsafe "$1.setSize($2, $3)"
    setSize'                :: GenericWidget -> Double -> Double -> IO ()

foreign import javascript unsafe "app.removeWidget($1)"
    removeWidget :: Int -> IO ()

setWidgetPosition :: UIWidget a => Vector2 Double -> a -> IO ()
setWidgetPosition (Vector2 x y) widget = setWidgetPosition' (pToJSVal widget) x y

updatePosition :: (IsDisplayObject b) => WidgetFile b -> Command UIRegistry.State ()
updatePosition file = performIO $ do
    let position = file ^. widget . widgetPosition
    w <- UIR.lookup $ file ^. objectId :: IO (GenericWidget)
    setWidgetPosition position w

updatePosition' :: WidgetId -> Vector2 Double -> IO ()
updatePosition' id pos = do
        w <- UIR.lookup $ id :: IO (GenericWidget)
        setWidgetPosition pos w

setSize :: (IsDisplayObject b) => WidgetId -> b -> IO ()
setSize id model = do
    let (Vector2 x y) = model ^. widgetSize
    w <- UIR.lookup $ id :: IO (GenericWidget)
    setSize' w x y

takeFocus :: a -> WidgetId -> Command Global.State ()
takeFocus _ id = Global.uiRegistry . UIRegistry.focusedWidget ?= id

startDrag :: Mouse.Event' -> WidgetId -> Command Global.State ()
startDrag event@(Mouse.Event eventType pos button keymods (Just (Mouse.EventWidget widgetId mat scene))) id = do
    camera <- use $ Global.camera . Camera.camera
    Global.uiRegistry . UIRegistry.dragState ?= DragState widgetId mat scene button keymods pos pos pos


whenChanged :: (Eq b) => a -> a -> Lens' a b -> IO () -> IO ()
whenChanged old new get action = if (old ^. get) /= (new ^. get) then action
                                                                 else return ()
