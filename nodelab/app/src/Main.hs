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
import           GHCJS.DOM.WebSocket (WebSocket, newWebSocket)
import           BatchConnector.Commands

import qualified Reactive.Plugins.Core.Network as CoreNetwork

import qualified Tmp.TypecheckerTest as Typechecker -- TODO: Remove

makeNetworkDescription :: forall t. Frameworks t => WebSocket -> Bool -> Moment t ()
makeNetworkDescription = CoreNetwork.makeNetworkDescription

main :: IO ()
main = do
    Typechecker.main  -- TODO: Remove
    sock <- newWebSocket "ws://0.0.0.0:8088" $ Just ([]::[String])
    initializeGl
    render
    enableLogging <- isLoggerEnabled
    eventNetwork  <- compile $ makeNetworkDescription sock enableLogging
    actuate eventNetwork

    triggerWindowResize
