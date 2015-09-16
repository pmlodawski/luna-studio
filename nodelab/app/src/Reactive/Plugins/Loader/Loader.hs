module Reactive.Plugins.Loader.Loader where

import           Utils.PreludePlus
import           JS.WebSocket
import           JS.Bindings        (getBackendAddress)
import qualified Reactive.Plugins.Loader.ProjectManager.Network as ProjectManager
import qualified Reactive.Plugins.Loader.Interpreter.Network    as Interpreter
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Batch.Workspace

runLoader :: WebSocket -> (Workspace -> IO ()) -> IO ()
runLoader socket callback = do
    backendAddr <- getBackendAddress
    connect socket backendAddr
    let interpreterCallback = Interpreter.run socket callback
    loaderNetwork <- compile $ ProjectManager.makeNetworkDescription interpreterCallback socket
    actuate loaderNetwork
