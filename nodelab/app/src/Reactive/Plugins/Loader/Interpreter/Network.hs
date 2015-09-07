module Reactive.Plugins.Loader.Interpreter.Network where

import Utils.PreludePlus
import Batch.Project
import Batch.Breadcrumbs
import BatchConnector.Commands
import BatchConnector.Connection

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Handlers

import           Data.Dynamic
import           Data.ByteString.Lazy      (ByteString)
import           JS.WebSocket
import qualified Event.Event               as Event
import           Event.Batch               as Batch
import           Event.Processors.Batch    (process)
import           BatchConnector.Connection
import           BatchConnector.Commands
import           BatchConnector.Updates
import           Batch.Workspace
import           Batch.Breadcrumbs

type Action = (IO (), Maybe Breadcrumbs)

react :: Project -> Event.Event Dynamic -> Maybe Action
react project (Event.Batch event) = Just $ reactToBatchEvent project event
react _       _                   = Nothing

reactToBatchEvent :: Project -> Batch.Event -> Action
reactToBatchEvent project (WorkspaceCreated crumbs) = handleWorkspaceCreation project crumbs
reactToBatchEvent project (ValueUpdate val)         = (print val, Nothing)
reactToBatchEvent _       _                         = (return (), Nothing)

handleWorkspaceCreation :: Project -> Breadcrumbs -> Action
handleWorkspaceCreation project crumbs = (setMain project crumbs, Just crumbs)

setMain :: Project -> Breadcrumbs -> IO ()
setMain project crumbs = do
    let lib = head $ project ^. libs
    sendMessage $ setMainPtr project (head $ project ^. libs) crumbs

makeNetworkDescription :: forall t. Frameworks t => Project -> WebSocket -> (Workspace -> IO ()) -> Moment t ()
makeNetworkDescription project socket callback = do
    webSocketE <- fromAddHandler $ webSocketHandler socket

    let batchE    = filterJust $ process <$> webSocketE
        actions   = filterJust $ react project <$> batchE
        crumbs    = filterJust $ snd <$> actions
        workspace = Workspace project (head $ project ^. libs) <$> crumbs

    reactimate $ fst <$> actions
    reactimate $ callback <$> workspace

run :: WebSocket -> (Workspace -> IO ()) -> Project -> IO ()
run socket callback project = do
    sendMessage $ setProjectId project
    sendMessage $ createMainFunction project (head $ project ^. libs)
    net <- compile $ makeNetworkDescription project socket callback
    actuate net
