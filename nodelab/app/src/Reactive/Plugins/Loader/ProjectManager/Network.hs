module Reactive.Plugins.Loader.ProjectManager.Network where

import Utils.PreludePlus

import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Handlers
import qualified Event.Event                                    as Event
import           JS.WebSocket                                   (WebSocket)
import           Reactive.Plugins.Loader.ProjectManager.Actions
import           Reactive.Plugins.Loader.ProjectManager.State   as State
import           Batch.Project

isReady :: State -> Bool
isReady state = state ^. connection == Ready

makeNetworkDescription :: forall t. Frameworks t => (Project -> IO ()) -> WebSocket -> Moment t ()
makeNetworkDescription callback conn = do
    webSocketE <- fromAddHandler $ webSocketHandler conn

    let updates :: Event t (Action -> Action)
        updates  = filterJust $ react <$> webSocketE

    let actions :: Event t Action
        actions  = accumE (return (), def) updates

    let readyStates :: Event t State
        readyStates  = filterE isReady $ fmap snd actions

    let project :: Event t Project
        project  = filterJust $ fmap (^. State.project) readyStates

    reactimate $ fst <$> actions
    reactimate $ print <$> project
    reactimate $ callback <$> project
