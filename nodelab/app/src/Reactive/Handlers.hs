{-# LANGUAGE OverloadedStrings #-}
module Reactive.Handlers where

import           Utils.PreludePlus         hiding (on)
import           Utils.Vector

import           GHCJS.DOM                 (currentDocument, currentWindow)
import qualified GHCJS.DOM.Document        as Document
import           GHCJS.DOM.Element         (Element, click, dblClick, keyDown, keyPress, keyUp, mouseDown, mouseMove,
                                            mouseUp, wheel)
import           GHCJS.DOM.EventM
import qualified GHCJS.DOM.KeyboardEvent   as KeyboardEvent
import qualified GHCJS.DOM.MouseEvent      as MouseEvent
import qualified GHCJS.DOM.UIEvent         as UIEvent
import qualified GHCJS.DOM.WheelEvent      as WheelEvent
import           GHCJS.DOM.Window          (getInnerHeight, getInnerWidth, resize)
import           GHCJS.Marshal.Pure        (pFromJSVal)
import           GHCJS.Prim                (fromJSString)
import qualified JS.ConnectionPen          as ConnectionPen
import qualified JS.CustomEvent            as CustomEvent
import qualified JS.TextEditor             as TextEditor
import qualified JS.WebSocket              as WebSocket

import qualified BatchConnector.Connection as Connection
import qualified Data.JSString             as JSString
import           Data.JSString.Text        (lazyTextFromJSString, lazyTextToJSString)
import qualified Event.Connection          as Connection
import qualified Event.ConnectionPen       as ConnectionPen
import qualified Event.CustomEvent         as CustomEvent
import qualified Event.Debug               as Debug
import           Event.Event
import qualified Event.Keyboard            as Keyboard
import qualified Event.Mouse               as Mouse
import qualified Event.NodeSearcher        as NodeSearcher
import qualified Event.TextEditor          as TextEditor
import qualified Event.Window              as Window
import           GHCJS.Marshal
import           JavaScript.Array          (JSArray)
import qualified JavaScript.Array          as JSArray
import           JS.NodeSearcher           (getAction, getExpression, getNode, nsEvent)
import           Object.UITypes            as Mouse
import qualified Object.Widget             as Widget
import           UI.Raycaster


data AddHandler a = AddHandler ((a -> IO ()) -> IO (IO ()))

foreign import javascript safe "app.getJSState()" getJSState :: IO JSState

backspace = 8

readKeyMods :: (MouseEvent.IsMouseEvent e) => EventM t e Keyboard.KeyMods
readKeyMods = do
    e      <- event
    shift  <- MouseEvent.getShiftKey e
    ctrl   <- MouseEvent.getCtrlKey  e
    alt    <- MouseEvent.getAltKey   e
    meta   <- MouseEvent.getMetaKey  e
    return $  Keyboard.KeyMods shift ctrl alt meta

readKeyMods' :: EventM t KeyboardEvent.KeyboardEvent Keyboard.KeyMods
readKeyMods' = do
    e      <- event
    shift  <- KeyboardEvent.getShiftKey e
    ctrl   <- KeyboardEvent.getCtrlKey  e
    alt    <- KeyboardEvent.getAltKey   e
    meta   <- KeyboardEvent.getMetaKey  e
    return $  Keyboard.KeyMods shift ctrl alt meta

readMousePos :: (MouseEvent.IsMouseEvent e) => EventM t e Mouse.MousePosition
readMousePos = do
    e <- event
    x <- MouseEvent.getClientX e
    y <- MouseEvent.getClientY e
    return $ Vector2 x y

uiWhichButton :: (UIEvent.IsUIEvent e) => EventM t e Mouse.MouseButton
uiWhichButton = uiWhich >>= return . Mouse.toMouseButton

eventObject :: IO (Maybe Element)
eventObject = do
    doc <- fromJust <$> currentDocument
    Document.getElementById doc (JSString.pack "canvas2d")

mouseHandler :: EventName Element MouseEvent.MouseEvent -> Mouse.Type -> AddHandler Event
mouseHandler event tag =
    AddHandler $ \h -> do
        window <- fromJust <$> eventObject
        window `on` event $ do
            mousePos        <- readMousePos
            button          <- uiWhichButton
            keyMods         <- readKeyMods
            objectId        <- liftIO $ readObjectId     mousePos
            scene           <- liftIO $ whichScene       objectId
            widgetMatrix    <- liftIO $ readWidgetMatrix objectId
            let maybeWidget  = do justObjectId        <- objectId
                                  justWidgetMatrix    <- widgetMatrix
                                  justScene           <- scene
                                  return $ Mouse.EventWidget justObjectId justWidgetMatrix justScene
            jsState <- liftIO getJSState
            liftIO . h $ Mouse jsState $ Mouse.Event tag mousePos button keyMods maybeWidget

mouseDownHandler     = mouseHandler mouseDown  Mouse.Pressed
mouseUpHandler       = mouseHandler mouseUp    Mouse.Released
mouseMovedHandler    = mouseHandler mouseMove  Mouse.Moved
mouseDblClickHandler = mouseHandler dblClick   Mouse.DblClicked

mouseWheelHandler :: AddHandler Event
mouseWheelHandler =
    AddHandler $ \h -> do
        window <- fromJust <$> eventObject
        window `on` wheel $ do
            preventDefault
            mousePos        <- readMousePos
            button          <- uiWhichButton
            keyMods         <- readKeyMods

            objectId        <- liftIO $ readObjectId     mousePos
            scene           <- liftIO $ whichScene       objectId
            widgetMatrix    <- liftIO $ readWidgetMatrix objectId
            delta <- do
                e <- event
                x <- WheelEvent.getDeltaX e
                y <- WheelEvent.getDeltaY e
                return $ Vector2 x y


            let maybeWidget  = do justObjectId        <- objectId
                                  justWidgetMatrix    <- widgetMatrix
                                  justScene           <- scene
                                  return $ Mouse.EventWidget justObjectId justWidgetMatrix justScene
            jsState <- liftIO getJSState
            liftIO . h $ Mouse jsState $ Mouse.Event (Mouse.Wheel delta) mousePos button keyMods maybeWidget

keyHandler :: EventName Element KeyboardEvent.KeyboardEvent -> EventM Element KeyboardEvent.KeyboardEvent Int -> Keyboard.Type -> AddHandler Event
keyHandler event getter tag = AddHandler $ \h -> do
    window <- fromJust <$> eventObject
    window `on` event $ do
        key     <- getter
        when (key == backspace) $ preventDefault
        keyMods <- readKeyMods'
        jsState <- liftIO getJSState
        liftIO . h $ Keyboard jsState $ Keyboard.Event tag (chr key) keyMods

keyPressedHandler :: AddHandler Event
keyPressedHandler = keyHandler keyPress uiCharCode Keyboard.Press
keyDownHandler    = keyHandler keyDown  uiKeyCode  Keyboard.Down
keyUpHandler      = keyHandler keyUp    uiKeyCode  Keyboard.Up

resizeHandler :: AddHandler Event
resizeHandler = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    window `on` resize $ liftIO $ do
        width  <- getInnerWidth  window
        height <- getInnerHeight window
        h $ Window $ Window.Resized width height

nodeSearcherHander :: AddHandler Event
nodeSearcherHander = AddHandler $ \h -> do
    window <- fromJust <$> currentWindow
    window `on` nsEvent $ do
        e      <- event
        action <- getAction e
        expr   <- getExpression e
        node   <- getNode e
        let maybeNode = if node == 0 then Nothing else Just node
        liftIO . h $ NodeSearcher $ NodeSearcher.Event action expr maybeNode

webSocketHandler :: WebSocket.WebSocket -> AddHandler Event
webSocketHandler conn = AddHandler $ \h -> do
    WebSocket.onOpen conn $ do
        h $ Connection Connection.Opened
    WebSocket.onMessage conn $ \event -> do
        payload <- WebSocket.getData event
        let frame = Connection.deserialize $ fromJSString payload
        mapM_ (h . Connection . Connection.Message) $ frame ^. Connection.messages
    WebSocket.onClose conn $ \event -> do
        code <- WebSocket.getCode event
        h $ Connection $ Connection.Closed code
    WebSocket.onError conn $ do
        h $ Connection Connection.Error

connectionPenHandler :: AddHandler Event
connectionPenHandler  = AddHandler $ \h -> do
    ConnectionPen.registerCallback $ \widgets -> do
        arr       <- return $ JSArray.toList (ConnectionPen.toJSArray widgets)
        widgetIds <- mapM fromJSValUnchecked arr :: IO [WidgetId]
        liftIO $ h $ ConnectionPen $ ConnectionPen.Segment widgetIds

textEditorHandler :: AddHandler Event
textEditorHandler  = AddHandler $ \h -> do
    TextEditor.registerCallback $ \code -> do
        codeStr <- return $ TextEditor.toJSString code
        liftIO $ h $ TextEditor $ TextEditor.CodeModified $ lazyTextFromJSString codeStr

customEventHandler :: AddHandler Event
customEventHandler  = AddHandler $ \h -> do
    CustomEvent.registerCallback $ \topic payload -> do
        liftIO $ h $ CustomEvent $ CustomEvent.RawEvent (JSString.unpack $ pFromJSVal topic) payload
