module Reactive.Plugins.Loader.Loader where

import           Utils.PreludePlus
import qualified JS.WebSocket      as WS
import           JS.WebSocket      (WebSocket)
import           JS.Config         (getBackendAddress)
import           JS.UI             (displayConnectionClosedMessage)

withActiveConnection :: (WebSocket -> IO ()) -> IO ()
withActiveConnection action = do
    addr   <- getBackendAddress
    socket <- WS.getWebSocket
    WS.onOpen  socket $ action socket
    WS.onClose socket $ const displayConnectionClosedMessage
    WS.onError socket displayConnectionClosedMessage
    WS.connect socket addr
