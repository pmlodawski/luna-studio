module Reactive.Plugins.Loader.Loader where

import           Utils.PreludePlus
import           JS.WebSocket
import qualified Reactive.Plugins.Loader.ProjectManager.Network as ProjectManager
import qualified Reactive.Plugins.Loader.Interpreter.Network    as Interpreter
import           Reactive.Banana
import           Reactive.Banana.Frameworks

runLoader :: WebSocket -> IO () -> IO ()
runLoader socket callback = do
    connect socket "ws://0.0.0.0:8088"
    let interpreterCallback = Interpreter.run socket callback
    loaderNetwork <- compile $ ProjectManager.makeNetworkDescription interpreterCallback socket
    actuate loaderNetwork
