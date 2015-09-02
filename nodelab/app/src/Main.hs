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
import qualified Tmp.TypecheckerTest             as Typechecker -- TODO: Remove
import           Reactive.Plugins.Loader.Loader

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

main :: IO ()
main = do
    socket <- getWebSocket
    enableBackend <- isBackendEnabled
    if enableBackend then runLoader socket (runMainNetwork socket) else runMainNetwork socket
