module Main where


--      _|      _|
--      _|_|    _|    _|_|    _|      _|      _|
--      _|  _|  _|  _|_|_|_|  _|      _|      _|
--      _|    _|_|  _|          _|  _|  _|  _|
--      _|      _|    _|_|_|      _|      _|



--      _|_|_|                _|
--      _|    _|  _|    _|  _|_|_|_|    _|_|
--      _|_|_|    _|    _|    _|      _|_|_|_|
--      _|    _|  _|    _|    _|      _|
--      _|_|_|      _|_|_|      _|_|    _|_|_|
--                      _|
--                  _|_|

--        _|_|                    _|
--      _|    _|  _|  _|_|    _|_|_|    _|_|    _|  _|_|
--      _|    _|  _|_|      _|    _|  _|_|_|_|  _|_|
--      _|    _|  _|        _|    _|  _|        _|
--        _|_|    _|          _|_|_|    _|_|_|  _|


-- http://www.network-science.de/ascii/

import           Utils.PreludePlus

import           Batch.Workspace                   (Workspace)
import qualified BatchConnector.Commands           as BatchCmd
import           JS.Config                         (getBackendAddress, isLoggerEnabled)
import           JS.UI                             (initializeGl, render, triggerWindowResize)
import           JS.WebSocket                      (WebSocket, connect, getWebSocket)
import           Reactive.Banana                   (Moment, compile)
import           Reactive.Banana.Frameworks        (Frameworks, actuate)
import           Reactive.Commands.Command         (Command, execCommand)
import qualified Reactive.Plugins.Core.Action.Init as Init
import qualified Reactive.Plugins.Core.Network     as CoreNetwork
import qualified Reactive.Plugins.Loader.Loader    as Loader
import           Reactive.State.Global             (State, initialState)
import           Utils.URIParser                   (getProjectName)


runMainNetwork :: WebSocket -> IO ()
runMainNetwork socket = do
    initializeGl
    render
    enableLogging <- isLoggerEnabled
    let (initActions, initState) = execCommand Init.initialize $ initialState
    initActions
    eventNetwork  <- compile $ CoreNetwork.makeNetworkDescription socket enableLogging initState
    actuate eventNetwork
    triggerWindowResize
    BatchCmd.listProjects

main :: IO ()
main = do
    maybeProjectName <- getProjectName
    let projectName = maybe "myFirstProject" id maybeProjectName
    Loader.withActiveConnection $ runMainNetwork

