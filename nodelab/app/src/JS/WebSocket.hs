{-# LANGUAGE JavaScriptFFI #-}

module JS.WebSocket (WebSocket
                    , onOpen
                    , onMessage
                    , getWebSocket
                    , connect
                    , send
                    , getData
                    ) where

import Utils.PreludePlus
import GHCJS.Types (JSVal, IsJSVal)
import GHCJS.Marshal.Pure (PToJSVal(..), PFromJSVal(..))
import GHCJS.Foreign.Callback
import GHCJS.Foreign
import Data.JSString

data WSRef
newtype WebSocket      = WebSocket      { unWebSocket      :: JSVal } deriving (PFromJSVal, PToJSVal)
newtype WSMessageEvent = WSMessageEvent { unWSMessageEvent :: JSVal } deriving (PFromJSVal, PToJSVal)

instance IsJSVal WebSocket
instance IsJSVal WSMessageEvent

foreign import javascript unsafe "$1.data"
    getData :: WSMessageEvent -> IO (JSVal)

foreign import javascript unsafe "app.websocket"
    getWebSocket :: IO WebSocket

foreign import javascript unsafe "$1.onOpen($2)"
    onOpen' :: WebSocket -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.unOnOpen($2)"
    unOnOpen' :: WebSocket -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.onMessage($2)"
    onMessage' :: WebSocket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.unOnMessage()"
    unOnMessage' :: WebSocket -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript unsafe "$1.send($2)"
    send :: WebSocket -> JSString -> IO ()

foreign import javascript unsafe "$1.connect($2)"
    connect' :: WebSocket -> JSString -> IO ()

connect :: WebSocket -> String -> IO ()
connect ws addr = connect' ws $ pack addr

onOpen :: WebSocket -> IO () -> IO (IO ())
onOpen ws callback = do
    wrappedCallback <- asyncCallback callback
    onOpen' ws wrappedCallback
    return $ unOnOpen' ws wrappedCallback >> releaseCallback wrappedCallback

onMessage :: WebSocket -> (WSMessageEvent -> IO ()) -> IO (IO ())
onMessage ws callback = do
    wrappedCallback <- asyncCallback1 (callback . pFromJSVal)
    onMessage' ws wrappedCallback
    return $ unOnMessage' ws wrappedCallback >> releaseCallback wrappedCallback

