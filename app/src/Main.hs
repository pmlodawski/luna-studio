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

import Reactive.Banana
import Reactive.Banana.Frameworks ( Frameworks, actuate )
import JS.Bindings
import GHCJS.Foreign ( toJSString )

import qualified Reactive.Plugins.Core.Network as CoreNetwork

makeNetworkDescription :: forall t. Frameworks t => Moment t ()
makeNetworkDescription = CoreNetwork.makeNetworkDescription

main :: IO ()
main = do
    -- Main.init
    -- Main.create(30)
    -- Main.render

    -- functionRef <- getFunctionNode
    -- showLabel functionRef (toJSString "Trolololo")
    -- renderExamplePlot functionRef
    -- setSelected functionRef 2



    eventNetwork <- compile makeNetworkDescription
    actuate eventNetwork

