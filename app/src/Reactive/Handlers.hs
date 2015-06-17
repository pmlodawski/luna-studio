module Reactive.Handlers where

import Control.Applicative
import Data.Char           ( chr )
import Data.Maybe          ( fromJust )
import GHCJS.DOM           ( currentWindow )
import GHCJS.DOM.DOMWindow ( IsDOMWindow
                           , domWindowOnclick
                           , domWindowOnmouseup
                           , domWindowOnkeypress
                           , domWindowOnmousedown
                           , domWindowOnmousemove
                           )
import GHCJS.DOM.EventM    ( EventM
                           , uiCharCode
                           , mouseClientXY
                           , mouseShiftKey
                           , mouseCtrlKey
                           , mouseAltKey
                           , mouseMetaKey
                           )
import qualified GHCJS.DOM.MouseEvent
import Reactive.Banana.Frameworks ( AddHandler(..), liftIO )

import Mouse.Event                ( Point(..), KeyMods(..), FunctionNode(..), Event, ObjectEvent )
import qualified Mouse.Event
import JS.Bindings

readKeyMods :: (IsDOMWindow self) => EventM GHCJS.DOM.MouseEvent.MouseEvent self KeyMods
readKeyMods = do
  shift <- mouseShiftKey
  ctrl  <- mouseCtrlKey
  alt   <- mouseAltKey
  meta  <- mouseMetaKey
  return $ KeyMods shift ctrl alt meta

readMousePos :: (IsDOMWindow self) => EventM GHCJS.DOM.MouseEvent.MouseEvent self Point
readMousePos = --convert <$> mouseClientXY 
  do
    (x, y) <- mouseClientXY
    return $ Point x y

mouseClickedHandler :: AddHandler ()
mouseClickedHandler = AddHandler $ \h -> do
  window <- fromJust <$> currentWindow
  domWindowOnclick window . liftIO $ h ()

mouseDownHandler :: AddHandler ObjectEvent
mouseDownHandler = AddHandler $ \h -> do
  w <- currentWindow
  domWindowOnmousedown (fromJust w) $ do
    mousePos <- readMousePos
    keyMods  <- readKeyMods
    liftIO $ do
      (nodeId, selected, x, y) <- getNodeAt (_x mousePos) (_y mousePos)
      h $ Mouse.Event.newWithObject True mousePos keyMods nodeId selected (Point x y)

mouseUpHandler :: AddHandler ObjectEvent
mouseUpHandler = AddHandler $ \h -> do
  w <- currentWindow
  domWindowOnmouseup (fromJust w) $ do
    mousePos <- readMousePos
    keyMods  <- readKeyMods
    liftIO $ do
      (nodeId, selected, x, y) <- getNodeAt (_x mousePos) (_y mousePos)
      h $ Mouse.Event.newWithObject False mousePos keyMods nodeId selected (Point x y)

mouseMovedHandler :: AddHandler Point
mouseMovedHandler = AddHandler $ \h -> do
  w <- currentWindow
  domWindowOnmousemove (fromJust w) $ do
    mousePos <- readMousePos
    liftIO $ h mousePos

keyPressedHandler :: AddHandler Char
keyPressedHandler = AddHandler $ \h -> do
  w <- currentWindow
  domWindowOnkeypress (fromJust w) $ do
    key <- uiCharCode    
    liftIO . h $ chr key
