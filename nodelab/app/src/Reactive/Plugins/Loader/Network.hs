module Reactive.Plugins.Loader.Network where

import Utils.PreludePlus

import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Handlers
import qualified Event.Event                            as Event
import           JS.WebSocket                           (WebSocket)
import           Reactive.Plugins.Loader.Action.Backend as Backend
import           Batch.Project

isReady :: Backend.State -> Bool
isReady state = state ^. Backend.connection == Backend.Ready

makeNetworkDescription :: forall t. Frameworks t => IO () -> WebSocket -> Moment t ()
makeNetworkDescription callback conn = do
    webSocketE <- fromAddHandler $ webSocketHandler conn

    let updates :: Event t (Backend.Action -> Backend.Action)
        updates  = filterJust $ Backend.react <$> webSocketE

    let actions :: Event t Backend.Action
        actions  = accumE (return (), def) updates

    let readyStates :: Event t Backend.State
        readyStates  = filterE isReady $ fmap snd actions

    let project :: Event t Project
        project  = filterJust $ fmap (^. Backend.project) readyStates

    reactimate $ fst <$> actions
    reactimate $ print <$> project
    reactimate $ callback <$ project
