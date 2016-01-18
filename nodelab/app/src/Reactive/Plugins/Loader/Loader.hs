module Reactive.Plugins.Loader.Loader where

import           Utils.PreludePlus
import qualified JS.WebSocket      as WS
import           JS.WebSocket      (WebSocket)
import           JS.Config         (getBackendAddress)
import           JS.UI             (displayConnectionClosedMessage)
{-import qualified Reactive.Plugins.Loader.ProjectManager.Network as ProjectManager-}
{-import qualified Reactive.Plugins.Loader.Interpreter.Network    as Interpreter-}
{-import           Reactive.Banana-}
{-import           Reactive.Banana.Frameworks-}
{-import           Batch.Workspace-}

-- runLoader :: String -> WebSocket -> (Workspace -> IO ()) -> IO ()
-- runLoader projectName socket callback = do
--     backendAddr <- getBackendAddress
--     connect socket backendAddr
--     let interpreterCallback = Interpreter.run socket callback
--     loaderNetwork <- compile $ ProjectManager.makeNetworkDescription interpreterCallback projectName socket
--     actuate loaderNetwork

withActiveConnection :: (WebSocket -> IO ()) -> IO ()
withActiveConnection action = do
    addr   <- getBackendAddress
    socket <- WS.getWebSocket
    WS.onOpen  socket $ action socket
    WS.onClose socket $ const displayConnectionClosedMessage
    WS.onError socket displayConnectionClosedMessage
    WS.connect socket addr
