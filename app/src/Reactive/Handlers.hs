{-# LANGUAGE OverloadedStrings #-}
module Reactive.Handlers where

import           Utils.PreludePlus hiding ( on )
import           Utils.Vector

import           Data.Dynamic        ( Dynamic )

import           GHCJS.DOM           ( currentWindow )
import           GHCJS.DOM.EventM
import qualified GHCJS.DOM.Document   as Document
import           GHCJS.DOM.Window      ( Window, mouseDown, mouseUp, mouseMove, resize, keyPress, keyDown, keyUp, getInnerWidth, getInnerHeight, click, dblClick )
import qualified GHCJS.DOM.MouseEvent as MouseEvent
import qualified GHCJS.DOM.UIEvent    as UIEvent


import           Reactive.Banana.Frameworks ( AddHandler(..), liftIO )

import           JS.Bindings
import           ThreeJS.Raycaster
import           JS.NodeSearcher      ( getAction, getExpression, nsEvent )
import           Object.Object
import qualified Object.Widget       as Widget
import qualified Event.Keyboard      as Keyboard
import qualified Event.Mouse         as Mouse
import qualified Event.Window        as Window
import qualified Event.NodeSearcher  as NodeSearcher
import qualified Object.Node         ( Node )
import           Event.Event
import           GHCJS.Marshal
import           JavaScript.Array ( JSArray )
import qualified JavaScript.Array as JSArray

readKeyMods :: (MouseEvent.IsMouseEvent e) => EventM t e Keyboard.KeyMods
readKeyMods = do
    e <- event
    shift <- MouseEvent.getShiftKey e
    ctrl  <- MouseEvent.getCtrlKey e
    alt   <- MouseEvent.getAltKey  e
    meta  <- MouseEvent.getMetaKey e
    return $ Keyboard.KeyMods shift ctrl alt meta

readMousePos :: (MouseEvent.IsMouseEvent e) => EventM t e Mouse.MousePosition
readMousePos = do
    e <- event
    x <- MouseEvent.getClientX e
    y <- MouseEvent.getClientY e
    return $ Vector2 x y


uiWhichButton :: (UIEvent.IsUIEvent e) => EventM t e Mouse.MouseButton
uiWhichButton = uiWhich >>= return . Mouse.toMouseButton

mouseHandler :: EventName Window MouseEvent.MouseEvent -> Mouse.Type ->  AddHandler (Event Dynamic)
mouseHandler event tag =
    AddHandler $ \h -> do
       window <- fromJust <$> currentWindow
       window `on` event $ do
        mousePos        <- readMousePos
        button          <- uiWhichButton
        keyMods         <- readKeyMods
        objectId        <- liftIO $ readObjectId     mousePos
        scene           <- liftIO $ whichScene       objectId
        widgetMatrix    <- liftIO $ readWidgetMatrix objectId
        let maybeWidget  = do justObjectId     <- objectId
                              justWidgetMatrix <- widgetMatrix
                              justScene        <- scene
                              return $ Mouse.EventWidget justObjectId justWidgetMatrix justScene

        liftIO . h $ Mouse $ Mouse.Event tag mousePos button keyMods maybeWidget

mouseDownHandler     = mouseHandler mouseDown  Mouse.Pressed
mouseUpHandler       = mouseHandler mouseUp    Mouse.Released
mouseMovedHandler    = mouseHandler mouseMove  Mouse.Moved
mouseClickHandler    = mouseHandler click      Mouse.Clicked
mouseDblClickHandler = mouseHandler dblClick   Mouse.DblClicked

resizeHandler :: AddHandler (Event Dynamic)
resizeHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    window `on` resize $ liftIO $ do
        width  <- getInnerWidth  window
        height <- getInnerHeight window
        h $ Window $ Window.Event Window.Resized width height

keyPressedHandler :: AddHandler (Event Dynamic)
keyPressedHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    window `on` keyPress $ do
        key <- uiCharCode
        liftIO . h $ Keyboard $ Keyboard.Event Keyboard.Press $ chr key

keyDownHandler :: AddHandler (Event Dynamic)
keyDownHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    window `on` keyDown $  do
        key <- uiKeyCode
        liftIO . h $ Keyboard $ Keyboard.Event Keyboard.Down $ chr key

keyUpHandler :: AddHandler (Event Dynamic)
keyUpHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    window `on` keyUp $ do
        key <- uiKeyCode
        liftIO . h $ Keyboard $ Keyboard.Event Keyboard.Up $ chr key

nodeSearcherHander :: AddHandler (Event Dynamic)
nodeSearcherHander = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    window `on` nsEvent $ do
        e      <- event
        action <- getAction e
        expr   <- getExpression e
        liftIO . h $ NodeSearcher $ NodeSearcher.Event action expr
