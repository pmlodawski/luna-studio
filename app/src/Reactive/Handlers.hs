module Reactive.Handlers where

import           Control.Applicative
import           Control.Lens
import           Data.Word
import           Data.Char           ( chr )
import           Data.Maybe          ( fromJust )
import           Data.Dynamic        ( Dynamic )
import           GHCJS.DOM           ( currentWindow )
import           GHCJS.DOM.DOMWindow ( IsDOMWindow
                                     , domWindowOnclick
                                     , domWindowOnmouseup
                                     , domWindowOnkeypress
                                     , domWindowOnmousedown
                                     , domWindowOnmousemove
                                     )
import           GHCJS.DOM.EventM    ( EventM
                                     , uiCharCode
                                     , mouseClientXY
                                     , mouseButton
                                     , mouseShiftKey
                                     , mouseCtrlKey
                                     , mouseAltKey
                                     , mouseMetaKey
                                     --, preventDefault
                                     )
import qualified GHCJS.DOM.MouseEvent
import           Reactive.Banana.Frameworks ( AddHandler(..), liftIO )

import           JS.Bindings
import           Object.Object
import qualified Event.Keyboard ( KeyMods(..), Event, newEvent )
import qualified Event.Mouse    ( Type(..), WithObjects, newWithObjects, newEvent )
import qualified Object.Node    ( Node )

readKeyMods :: (IsDOMWindow self) => EventM GHCJS.DOM.MouseEvent.MouseEvent self Event.Keyboard.KeyMods
readKeyMods = do
  shift <- mouseShiftKey
  ctrl  <- mouseCtrlKey
  alt   <- mouseAltKey
  meta  <- mouseMetaKey
  return $ Event.Keyboard.KeyMods shift ctrl alt meta

readButton :: (IsDOMWindow self) => EventM GHCJS.DOM.MouseEvent.MouseEvent self Word
readButton = mouseButton

readMousePos :: (IsDOMWindow self) => EventM GHCJS.DOM.MouseEvent.MouseEvent self Point
readMousePos = --convert <$> mouseClientXY
  do
    (x, y) <- mouseClientXY
    return $ Point x y


newMouseWithObjects :: Event.Mouse.Type -> Point -> Word -> Event.Keyboard.KeyMods -> [Object Dynamic] -> Event.Mouse.WithObjects Dynamic
newMouseWithObjects eventType point button keyMods objects =
  Event.Mouse.newWithObjects (Event.Mouse.newEvent eventType point button keyMods) objects

mouseDownHandler :: AddHandler (Event.Mouse.WithObjects Dynamic)
mouseDownHandler = AddHandler $ \h -> do
  window <- fromJust <$> currentWindow
  domWindowOnmousedown window $ do
    mousePos <- readMousePos
    button   <- readButton
    keyMods  <- readKeyMods
    liftIO $ do
      objects <- getObjectsAt (mousePos ^. x) (mousePos ^. y)
      h $ newMouseWithObjects Event.Mouse.Pressed mousePos button keyMods objects

mouseUpHandler :: AddHandler (Event.Mouse.WithObjects Dynamic)
mouseUpHandler = AddHandler $ \h -> do
  window <- fromJust <$> currentWindow
  domWindowOnmouseup window $ do
    mousePos <- readMousePos
    button   <- readButton
    keyMods  <- readKeyMods
    liftIO $ do
      objects <- getObjectsAt (mousePos ^. x) (mousePos ^. y)
      h $ newMouseWithObjects Event.Mouse.Released mousePos button keyMods objects

mouseMovedHandler :: AddHandler (Event.Mouse.WithObjects Dynamic)
mouseMovedHandler = AddHandler $ \h -> do
  window <- fromJust <$> currentWindow
  domWindowOnmousemove window $ do
    mousePos <- readMousePos
    button   <- readButton
    keyMods  <- readKeyMods
    liftIO $ do
      objects <- getObjectsAt (mousePos ^. x) (mousePos ^. y)
      h $ newMouseWithObjects Event.Mouse.Moved mousePos button keyMods objects

keyPressedHandler :: AddHandler Event.Keyboard.Event
keyPressedHandler = AddHandler $ \h -> do
  window <- fromJust <$> currentWindow
  domWindowOnkeypress window $ do
    key <- uiCharCode
    liftIO . h $ Event.Keyboard.newEvent $ chr key
