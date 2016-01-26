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

import           Reactive.Banana                (Moment, compile)
import           Reactive.Banana.Frameworks     (Frameworks, actuate)
import qualified Reactive.Plugins.Core.Network  as CoreNetwork
import qualified Reactive.Plugins.Loader.Loader as Loader
import           JS.UI                          (initializeGl, render, triggerWindowResize)
import           JS.WebSocket                   (WebSocket, getWebSocket, connect)
import           JS.Config                      (isLoggerEnabled, getBackendAddress)
import qualified BatchConnector.Commands        as BatchCmd
import           Batch.Workspace                (Workspace)
import           Utils.URIParser                (getProjectName)
import           FakeMock                       (fakeWorkspace)


makeNetworkDescription :: forall t. Frameworks t => WebSocket -> Bool -> Workspace -> Moment t ()
makeNetworkDescription = CoreNetwork.makeNetworkDescription

runMainNetwork :: Workspace -> WebSocket -> IO ()
runMainNetwork workspace socket = do
    initializeGl
    render
    enableLogging <- isLoggerEnabled
    eventNetwork  <- compile $ makeNetworkDescription socket enableLogging workspace
    actuate eventNetwork
    triggerWindowResize
    BatchCmd.listProjects

main :: IO ()
main = do
    maybeProjectName <- getProjectName
    let projectName = maybe "myFirstProject" id maybeProjectName
    Loader.withActiveConnection $ runMainNetwork fakeWorkspace

