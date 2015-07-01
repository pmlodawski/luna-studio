module Main where


-- _|      _|
-- _|_|    _|    _|_|    _|      _|      _|
-- _|  _|  _|  _|_|_|_|  _|      _|      _|
-- _|    _|_|  _|          _|  _|  _|  _|
-- _|      _|    _|_|_|      _|      _|



-- _|_|_|                _|
-- _|    _|  _|    _|  _|_|_|_|    _|_|
-- _|_|_|    _|    _|    _|      _|_|_|_|
-- _|    _|  _|    _|    _|      _|
-- _|_|_|      _|_|_|      _|_|    _|_|_|
--                 _|
--             _|_|

--   _|_|                    _|
-- _|    _|  _|  _|_|    _|_|_|    _|_|    _|  _|_|
-- _|    _|  _|_|      _|    _|  _|_|_|_|  _|_|
-- _|    _|  _|        _|    _|  _|        _|
--   _|_|    _|          _|_|_|    _|_|_|  _|


-- http://www.network-science.de/ascii/

import           Reactive.Banana
import           Reactive.Banana.Frameworks ( Frameworks, actuate )
import qualified JS.Bindings as JS
import           GHCJS.Foreign ( toJSString )

import qualified Reactive.Plugins.Core.Network as CoreNetwork

makeNetworkDescription :: forall t. Frameworks t => Moment t ()
makeNetworkDescription = CoreNetwork.makeNetworkDescription

main :: IO ()
main = do
    -- JS.init
    -- JS.create(30)
    -- JS.render

    -- functionRef <- getFunctionNode
    -- showLabel functionRef (toJSString "Trolololo")
    -- renderExamplePlot functionRef
    -- setSelected functionRef 2



    eventNetwork <- compile makeNetworkDescription
    actuate eventNetwork

