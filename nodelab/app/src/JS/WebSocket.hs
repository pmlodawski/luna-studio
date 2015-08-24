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
import GHCJS.Types
import GHCJS.Foreign.Callback
import GHCJS.Foreign
import Data.JSString

data WSRef
type WebSocket = JSRef WSRef
data WSMessageEvent

foreign import javascript unsafe "$1.data"
    getData :: JSRef WSMessageEvent -> IO (JSRef ())

foreign import javascript unsafe "app.websocket"
    getWebSocket :: IO WebSocket

foreign import javascript unsafe "$1.onOpen($2)"
    onOpen' :: WebSocket -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$1.unOnOpen()"
    unOnOpen' :: WebSocket -> IO ()

foreign import javascript unsafe "$1.onMessage($2)"
    onMessage' :: WebSocket -> Callback (JSRef WSMessageEvent -> IO ()) -> IO ()

foreign import javascript unsafe "$1.unOnMessage()"
    unOnMessage' :: WebSocket -> IO ()

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
    return $ unOnOpen' ws >> releaseCallback wrappedCallback

onMessage :: WebSocket -> (JSRef WSMessageEvent -> IO ()) -> IO (IO ())
onMessage ws callback = do
    wrappedCallback <- asyncCallback1 callback
    onMessage' ws wrappedCallback
    return $ unOnMessage' ws >> releaseCallback wrappedCallback

