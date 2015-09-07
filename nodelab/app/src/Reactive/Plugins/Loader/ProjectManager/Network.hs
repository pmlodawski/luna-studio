module Reactive.Plugins.Loader.ProjectManager.Network where

import           Utils.PreludePlus

import           Reactive.Banana
import           Reactive.Banana.Frameworks
import           Reactive.Handlers
import qualified Event.Event                                    as Event
import           JS.WebSocket                                   (WebSocket)
import           Reactive.Plugins.Loader.ProjectManager.Actions
import           Reactive.Plugins.Loader.ProjectManager.State   as State
import           Event.Processors.Batch                         (process)
import           Batch.Project

readyProject :: State -> Maybe Project
readyProject (Ready project) = Just project
readyProject _               = Nothing

makeNetworkDescription :: forall t. Frameworks t => (Project -> IO ()) -> WebSocket -> Moment t ()
makeNetworkDescription callback conn = do
    webSocketE <- fromAddHandler $ webSocketHandler conn
    let batchE = filterJust $ process <$> webSocketE

    let updates :: Event t (Action -> Action)
        updates  = filterJust $ react <$> unions [batchE, webSocketE]

    let actions :: Event t Action
        actions  = accumE (return (), def) updates

    let states :: Event t State
        states  = snd <$> actions

    let project :: Event t Project
        project  = filterJust $ readyProject <$> states

    reactimate $ fst <$> actions
    reactimate $ print <$> project
    reactimate $ callback <$> project
