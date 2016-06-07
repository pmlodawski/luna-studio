{-# LANGUAGE Rank2Types #-}

module UI.Generic where

import           GHCJS.Marshal.Pure        (pToJSVal)
import           GHCJS.Types               (JSVal)
import           Utils.PreludePlus
import           Utils.Vector

import qualified Event.Mouse               as Mouse
import           Object.Widget             (DragState (..), IsDisplayObject, WidgetFile, WidgetId, objectId, widget,
                                            widgetPosition)
import           Reactive.Commands.Command (Command, performIO)
import qualified Reactive.State.Camera     as Camera
import qualified Reactive.State.Global     as Global
import qualified Reactive.State.UIRegistry as UIRegistry
import qualified UI.Registry               as UIR
import           UI.Widget                 (GenericWidget (..), UIWidget)


foreign import javascript safe "$1.mesh.position.x = $2; $1.mesh.position.y = $3; $1.widgetMoved()"
    setWidgetPosition'      :: JSVal -> Double -> Double -> IO ()

foreign import javascript safe "$1.setSize($2, $3)"
    setSize'                :: GenericWidget -> Double -> Double -> IO ()

foreign import javascript safe "app.removeWidget($1)"
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

setSize :: WidgetId -> Vector2 Double -> IO ()
setSize id (Vector2 x y) = do
    w <- UIR.lookup $ id :: IO (GenericWidget)
    setSize' w x y

defaultResize :: WidgetId -> Vector2 Double -> a -> Command UIRegistry.State ()
defaultResize id size _ = performIO $ setSize id size

startDrag :: Mouse.Event' -> WidgetId -> Command Global.State ()
startDrag (Mouse.Event _ pos button keymods (Just (Mouse.EventWidget widgetId mat scene))) id = do
    camera <- use $ Global.camera . Camera.camera
    Global.uiRegistry . UIRegistry.dragState ?= DragState widgetId mat scene button keymods pos pos pos


whenChanged :: (Eq b, Monad m) => a -> a -> Getter a b -> m () -> m ()
whenChanged old new get action = if (old ^. get) /= (new ^. get) then action
                                                                 else return ()
