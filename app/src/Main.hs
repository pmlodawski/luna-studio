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

import qualified Reactive.Plugins.Core.Network as CoreNetwork

makeNetworkDescription :: forall t. Frameworks t => Bool -> Moment t ()
makeNetworkDescription = CoreNetwork.makeNetworkDescription

main :: IO ()
main = do
    initializeGl
    render
    enableLogging <- isLoggerEnabled
    eventNetwork  <- compile $ makeNetworkDescription enableLogging
    actuate eventNetwork

    triggerWindowResize
