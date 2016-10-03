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

import           Data.DateTime                     (getCurrentTime)
import qualified Data.Set                          as Set
import           System.Random                     (newStdGen)
import           Utils.PreludePlus

import qualified Batch.Workspace                   as Workspace
import qualified BatchConnector.Commands           as BatchCmd
import           Control.Concurrent.MVar
import qualified JS.GraphLocation                  as GraphLocation
import           JS.UI                             (initializeGl, render, triggerWindowResize)
import           JS.UUID                           (generateUUID)
import           JS.Tutorial                       (shouldRunTutorial)
import           JS.WebSocket                      (WebSocket)
import           Reactive.Commands.Command         (execCommand)
import qualified Reactive.Plugins.Core.Action.Init as Init
import qualified Reactive.Plugins.Core.Network     as CoreNetwork
import qualified Reactive.Plugins.Loader.Loader    as Loader
import           Reactive.State.Global             (initialState)
import qualified Reactive.State.Global             as Global
import           JS.Tutorial                       (showStep)

runMainNetwork :: WebSocket -> IO ()
runMainNetwork socket = do
    initializeGl
    render

    lastLocation <- GraphLocation.loadLocation

    random <- newStdGen
    projectListRequestId <- generateUUID
    clientId             <- generateUUID
    initTime             <- getCurrentTime
    tutorial'            <- shouldRunTutorial
    let tutorial = if tutorial' then Just 0 else Nothing

    withJust tutorial $ \step -> showStep step


    let initState = initialState initTime clientId random tutorial & Global.workspace . Workspace.lastUILocation .~ lastLocation
                                                                   & Global.pendingRequests %~ Set.insert projectListRequestId
    let (initActions, initState') = execCommand Init.initialize initState
    initActions

    state <- newMVar initState'
    CoreNetwork.makeNetworkDescription socket state
    triggerWindowResize

    BatchCmd.listProjects projectListRequestId

main :: IO ()
main = Loader.withActiveConnection runMainNetwork

