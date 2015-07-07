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
                                     , domWindowOnkeydown
                                     , domWindowOnkeypress
                                     , domWindowOnkeyup
                                     , domWindowOnmousedown
                                     , domWindowOnmousemove
                                     )
import           GHCJS.DOM.EventM    ( EventM
                                     , uiCharCode
                                     , uiKeyCode
                                     , uiWhich
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
import           JS.NodeSearcher
import           GHCJS.Foreign

import           Object.Object
import qualified Event.Keyboard ( KeyMods(..), Event, newEvent, Type(..) )
import qualified Event.Mouse    ( Type(..), newWithObjects, newEvent )
import qualified Event.NodeSearcher ( newEvent )
import qualified Object.Node    ( Node )
import           Event.Event

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

readKeyMods :: (IsDOMWindow self) => EventM GHCJS.DOM.MouseEvent.MouseEvent self Event.Keyboard.KeyMods
readKeyMods = do
    shift <- mouseShiftKey
    ctrl  <- mouseCtrlKey
    alt   <- mouseAltKey
    meta  <- mouseMetaKey
    return $ Event.Keyboard.KeyMods shift ctrl alt meta

readButton :: (IsDOMWindow self) => EventM GHCJS.DOM.MouseEvent.MouseEvent self Int
readButton = uiWhich

readMousePos :: (IsDOMWindow self) => EventM GHCJS.DOM.MouseEvent.MouseEvent self Point
readMousePos = --convert <$> mouseClientXY
    do
        (x, y) <- mouseClientXY
        return $ Point x y


newMouseWithObjects :: Event.Mouse.Type -> Point -> Int -> Event.Keyboard.KeyMods -> [Object Dynamic] -> Event Dynamic
newMouseWithObjects eventType point button keyMods objects =
    Mouse $ Event.Mouse.newWithObjects (Event.Mouse.newEvent eventType point button keyMods) objects

mouseDownHandler :: AddHandler (Event Dynamic)
mouseDownHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    domWindowOnmousedown window $ do
        mousePos <- readMousePos
        button   <- readButton
        keyMods  <- readKeyMods
        liftIO $ do
            objects <- getObjectsAt (mousePos ^. x) (mousePos ^. y)
            h $ newMouseWithObjects Event.Mouse.Pressed mousePos button keyMods objects

mouseUpHandler :: AddHandler (Event Dynamic)
mouseUpHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    domWindowOnmouseup window $ do
        mousePos <- readMousePos
        button   <- readButton
        keyMods  <- readKeyMods
        liftIO $ do
            objects <- getObjectsAt (mousePos ^. x) (mousePos ^. y)
            h $ newMouseWithObjects Event.Mouse.Released mousePos button keyMods objects

mouseMovedHandler :: AddHandler (Event Dynamic)
mouseMovedHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    domWindowOnmousemove window $ do
        mousePos <- readMousePos
        button   <- readButton
        keyMods  <- readKeyMods
        liftIO $ do
            objects <- getObjectsAt (mousePos ^. x) (mousePos ^. y)
            h $ newMouseWithObjects Event.Mouse.Moved mousePos button keyMods objects

keyPressedHandler :: AddHandler (Event Dynamic)
keyPressedHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    domWindowOnkeypress window $ do
        key <- uiCharCode
        liftIO . h $ Keyboard $ (Event.Keyboard.newEvent Event.Keyboard.Press (chr key))

keyDownHandler :: AddHandler (Event Dynamic)
keyDownHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    domWindowOnkeydown window $ do
        key <- uiKeyCode
        liftIO . h $ Keyboard $ (Event.Keyboard.newEvent Event.Keyboard.Down (chr key))

keyUpHandler :: AddHandler (Event Dynamic)
keyUpHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    domWindowOnkeyup window $ do
        key <- uiKeyCode
        liftIO . h $ Keyboard $ (Event.Keyboard.newEvent Event.Keyboard.Up (chr key))

nodeSearcherHander :: AddHandler (Event Dynamic)
nodeSearcherHander = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    nodeSearcherOnEvent window $ do
        action <- nsAction
        expr   <- nsExpression
        liftIO $ do
             -- putStrLn $ "Hello from haskell world, you asked for " ++ (Text.unpack action) ++ " => " ++ (Text.unpack expr)
             h $ NodeSearcher $ Event.NodeSearcher.newEvent action expr

