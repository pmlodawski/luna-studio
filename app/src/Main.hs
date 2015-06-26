module Main where

import Reactive.Banana
import Reactive.Banana.Frameworks ( Frameworks, actuate )

import qualified Reactive.Plugins.Core.Network as CoreNetwork

makeNetworkDescription :: forall t. Frameworks t => Moment t ()
makeNetworkDescription = CoreNetwork.makeNetworkDescription

main :: IO ()
main = do
    -- Main.init
    -- Main.create(30)
    -- Main.render
    eventNetwork <- compile makeNetworkDescription
    actuate eventNetwork

