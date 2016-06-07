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

import qualified Data.Set                          as Set
import           Utils.PreludePlus
import           System.Random (newStdGen)

import qualified Batch.Workspace                   as Workspace
import qualified BatchConnector.Commands           as BatchCmd
import           Control.Concurrent.MVar
import qualified JS.GraphLocation                  as GraphLocation
import           JS.UI                             (initializeGl, render, triggerWindowResize)
import           JS.UUID                           (generateUUID)
import           JS.WebSocket                      (WebSocket)
import           Reactive.Commands.Command         (execCommand)
import qualified Reactive.Plugins.Core.Action.Init as Init
import qualified Reactive.Plugins.Core.Network     as CoreNetwork
import qualified Reactive.Plugins.Loader.Loader    as Loader
import           Reactive.State.Global             (initialState)
import qualified Reactive.State.Global             as Global


runMainNetwork :: WebSocket -> IO ()
runMainNetwork socket = do
    initializeGl
    render

    lastLocation <- GraphLocation.loadLocation

    random <- newStdGen
    projectListRequestId <- generateUUID
    let initState = initialState random & Global.workspace . Workspace.lastUILocation .~ lastLocation
                                        & Global.pendingRequests %~ Set.insert projectListRequestId
    let (initActions, initState') = execCommand Init.initialize initState
    initActions

    state <- newMVar initState'
    CoreNetwork.makeNetworkDescription socket state
    triggerWindowResize

    BatchCmd.listProjects projectListRequestId

main :: IO ()
main = Loader.withActiveConnection runMainNetwork

