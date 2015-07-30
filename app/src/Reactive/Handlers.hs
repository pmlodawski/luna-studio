{-# LANGUAGE OverloadedStrings #-}
module Reactive.Handlers where

import           Utils.PreludePlus hiding ( on )
import           Utils.Vector

import           Data.Dynamic        ( Dynamic )

import           GHCJS.DOM           ( currentWindow )
import           GHCJS.DOM.EventM
import qualified GHCJS.DOM.Document   as Document
import           GHCJS.DOM.Window      ( mouseDown, mouseUp, mouseMove, resize, keyPress, keyDown, keyUp, getInnerWidth, getInnerHeight, click, dblClick )
import qualified GHCJS.DOM.MouseEvent as MouseEvent
import qualified GHCJS.DOM.UIEvent    as UIEvent


import           Reactive.Banana.Frameworks ( AddHandler(..), liftIO )

import           JS.Bindings
import           JS.NodeSearcher      ( getAction, getExpression, nsEvent )
import           Object.Object
import qualified Event.Keyboard      as Keyboard
import qualified Event.Mouse         as Mouse
import qualified Event.Window        as Window
import qualified Event.NodeSearcher  as NodeSearcher
import qualified Object.Node         ( Node )
import           Event.Event

readKeyMods = do
    e <- event
    shift <- MouseEvent.getShiftKey e
    ctrl  <- MouseEvent.getCtrlKey e
    alt   <- MouseEvent.getAltKey  e
    meta  <- MouseEvent.getMetaKey e
    return $ Keyboard.KeyMods shift ctrl alt meta


readMousePos =  do
    e <- event
    x <- MouseEvent.getClientX e
    y <- MouseEvent.getClientY e
    return $ Vector2 x y

uiWhichButton = mouseButton >>= return . Mouse.toMouseButton

mouseDownHandler :: AddHandler (Event Dynamic)
mouseDownHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    window `on` mouseDown $ do
        mousePos <- readMousePos
        button   <- uiWhichButton
        keyMods  <- readKeyMods
        liftIO . h $ Mouse $ Mouse.Event Mouse.Pressed mousePos button keyMods

mouseUpHandler :: AddHandler (Event Dynamic)
mouseUpHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    window `on` mouseUp $ do
        mousePos <- readMousePos
        button   <- uiWhichButton
        keyMods  <- readKeyMods
        liftIO . h $ Mouse $ Mouse.Event Mouse.Released mousePos button keyMods

mouseMovedHandler :: AddHandler (Event Dynamic)
mouseMovedHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    window `on` mouseMove $ do
        mousePos <- readMousePos
        button   <- uiWhichButton
        keyMods  <- readKeyMods
        liftIO . h $ Mouse $ Mouse.Event Mouse.Moved mousePos button keyMods

mouseClickHandler :: AddHandler (Event Dynamic)
mouseClickHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    window `on` click $ do
        mousePos <- readMousePos
        button   <- uiWhichButton
        keyMods  <- readKeyMods
        liftIO . h $ Mouse $ Mouse.Event Mouse.Clicked mousePos button keyMods

mouseDblClickHandler :: AddHandler (Event Dynamic)
mouseDblClickHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    window `on` dblClick $ do
        mousePos <- readMousePos
        button   <- uiWhichButton
        keyMods  <- readKeyMods
        liftIO . h $ Mouse $ Mouse.Event Mouse.DblClicked mousePos button keyMods

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
