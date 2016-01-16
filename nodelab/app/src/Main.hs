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

import           Control.Concurrent             (threadDelay)
import           Reactive.Banana                (Moment, compile)
import           Reactive.Banana.Frameworks     (Frameworks, actuate)
import qualified Reactive.Plugins.Core.Network  as CoreNetwork
import           JS.UI                          (initializeGl, render, triggerWindowResize)
import           JS.WebSocket                   (WebSocket, getWebSocket, connect)
import           JS.Config                      (isLoggerEnabled, getBackendAddress)
import qualified BatchConnector.Commands        as BatchCmd
import           Batch.Workspace                (Workspace)
import           Utils.URIParser                (getProjectName)
import           FakeMock                       (fakeWorkspace)


makeNetworkDescription :: forall t. Frameworks t => WebSocket -> Bool -> Workspace -> Moment t ()
makeNetworkDescription = CoreNetwork.makeNetworkDescription

runMainNetwork :: WebSocket -> Workspace -> IO ()
runMainNetwork socket workspace = do
    initializeGl
    render
    enableLogging <- isLoggerEnabled
    eventNetwork  <- compile $ makeNetworkDescription socket enableLogging workspace
    threadDelay 1000 -- TODO: workaround to prevent Failed to execute 'send' on 'WebSocket': Still in CONNECTING state.
    actuate eventNetwork
    triggerWindowResize
    BatchCmd.getProgram workspace

main :: IO ()
main = do
    socket <- getWebSocket
    backendAddr <- getBackendAddress
    connect socket backendAddr
    maybeProjectName <- getProjectName
    let projectName = maybe "myFirstProject" id maybeProjectName
    runMainNetwork socket fakeWorkspace

