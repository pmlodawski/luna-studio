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

import           Reactive.Banana
import           Reactive.Banana.Frameworks ( Frameworks, actuate )
import           JS.Bindings
import           JS.WebSocket
import           BatchConnector.Commands

import qualified Reactive.Plugins.Core.Network   as CoreNetwork
import qualified Reactive.Plugins.Loader.Network as LoaderNetwork

import qualified Tmp.TypecheckerTest as Typechecker -- TODO: Remove

makeNetworkDescription :: forall t. Frameworks t => WebSocket -> Bool -> Moment t ()
makeNetworkDescription = CoreNetwork.makeNetworkDescription

runMainNetwork :: WebSocket -> IO ()
runMainNetwork socket = do
    Typechecker.main  -- TODO: Remove
    initializeGl
    render
    enableLogging <- isLoggerEnabled
    eventNetwork  <- compile $ makeNetworkDescription socket enableLogging
    actuate eventNetwork
    triggerWindowResize

runLoaderNetwork :: WebSocket -> IO ()
runLoaderNetwork socket = do
    connect socket "ws://0.0.0.0:8088"
    loaderNetwork <- compile $ LoaderNetwork.makeNetworkDescription (runMainNetwork socket) socket
    actuate loaderNetwork


main :: IO ()
main = do
    socket <- getWebSocket
    enableBackend <- isBackendEnabled
    if enableBackend then runLoaderNetwork socket else runMainNetwork socket
