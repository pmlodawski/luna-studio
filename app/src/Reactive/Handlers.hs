module Reactive.Handlers where

import           Control.Applicative
import           Control.Lens
import           Data.Word
import           Data.Char           ( chr )
import           Data.Maybe          ( fromJust )
import           Data.Dynamic        ( Dynamic )
import           GHCJS.DOM           ( currentWindow )
import           GHCJS.DOM.DOMWindow
import           GHCJS.DOM.EventM
import qualified GHCJS.DOM.MouseEvent
import           Reactive.Banana.Frameworks ( AddHandler(..), liftIO )

import           JS.Bindings
import           JS.NodeSearcher
import           GHCJS.Foreign

import           Object.Object
import qualified Event.Keyboard      as Keyboard
import qualified Event.Mouse         as Mouse
import qualified Event.Window        as Window
import qualified Event.NodeSearcher  as NodeSearcher
import qualified Object.Node         ( Node )
import           Event.Event

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

readKeyMods :: (IsDOMWindow self) => EventM GHCJS.DOM.MouseEvent.MouseEvent self Keyboard.KeyMods
readKeyMods = do
    shift <- mouseShiftKey
    ctrl  <- mouseCtrlKey
    alt   <- mouseAltKey
    meta  <- mouseMetaKey
    return $ Keyboard.KeyMods shift ctrl alt meta

readButton :: (IsDOMWindow self) => EventM GHCJS.DOM.MouseEvent.MouseEvent self Int
readButton = uiWhich

readMousePos :: (IsDOMWindow self) => EventM GHCJS.DOM.MouseEvent.MouseEvent self Point
readMousePos = --convert <$> mouseClientXY
    do
        (x, y) <- mouseClientXY
        return $ Point x y

mouseDownHandler :: AddHandler (Event Dynamic)
mouseDownHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    domWindowOnmousedown window $ do
        mousePos <- readMousePos
        button   <- readButton
        keyMods  <- readKeyMods
        liftIO . h $ Mouse $ Mouse.Event Mouse.Pressed mousePos button keyMods

mouseUpHandler :: AddHandler (Event Dynamic)
mouseUpHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    domWindowOnmouseup window $ do
        mousePos <- readMousePos
        button   <- readButton
        keyMods  <- readKeyMods
        liftIO . h $ Mouse $ Mouse.Event Mouse.Released mousePos button keyMods

mouseMovedHandler :: AddHandler (Event Dynamic)
mouseMovedHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    domWindowOnmousemove window $ do
        mousePos <- readMousePos
        button   <- readButton
        keyMods  <- readKeyMods
        liftIO . h $ Mouse $ Mouse.Event Mouse.Moved mousePos button keyMods

resizeHandler :: AddHandler (Event Dynamic)
resizeHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    handler domWindowOnload   h window
    handler domWindowOnresize h window
    where
        handler domWindowOn h window = domWindowOn window $ liftIO $ do
            width  <- domWindowGetInnerWidth  window
            height <- domWindowGetInnerHeight window
            h $ Window $ Window.Event Window.Resized width height

keyPressedHandler :: AddHandler (Event Dynamic)
keyPressedHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    domWindowOnkeypress window $ do
        key <- uiCharCode
        liftIO . h $ Keyboard $ Keyboard.Event Keyboard.Press $ chr key

keyDownHandler :: AddHandler (Event Dynamic)
keyDownHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    domWindowOnkeydown window $ do
        key <- uiKeyCode
        liftIO . h $ Keyboard $ Keyboard.Event Keyboard.Down $ chr key

keyUpHandler :: AddHandler (Event Dynamic)
keyUpHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    domWindowOnkeyup window $ do
        key <- uiKeyCode
        liftIO . h $ Keyboard $ Keyboard.Event Keyboard.Up $ chr key

nodeSearcherHander :: AddHandler (Event Dynamic)
nodeSearcherHander = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    nodeSearcherOnEvent window $ do
        action <- nsAction
        expr   <- nsExpression
        liftIO $ do
             -- putStrLn $ "Hello from haskell world, you asked for " ++ (Text.unpack action) ++ " => " ++ (Text.unpack expr)
             h $ NodeSearcher $ NodeSearcher.Event action expr

