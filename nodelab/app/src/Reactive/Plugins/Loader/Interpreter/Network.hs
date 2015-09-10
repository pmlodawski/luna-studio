module Reactive.Plugins.Loader.Interpreter.Network where

import Utils.PreludePlus
import Batch.Project
import Batch.Breadcrumbs
import BatchConnector.Commands as BatchCmd

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Handlers

import           Data.Dynamic
import           Data.ByteString.Lazy      (ByteString)
import           JS.WebSocket
import qualified Event.Event               as Event
import           Event.Batch               as Batch
import           Event.Processors.Batch    (process)
import           BatchConnector.Updates
import           Batch.Workspace
import           Batch.Breadcrumbs

type Action = (IO (), Maybe Breadcrumbs)

react :: Project -> Event.Event Dynamic -> Maybe Action
react project (Event.Batch event) = Just $ reactToBatchEvent project event
react _       _                   = Nothing

reactToBatchEvent :: Project -> Batch.Event -> Action
reactToBatchEvent project (WorkspaceCreated crumbs) = handleWorkspaceCreation project crumbs
reactToBatchEvent project ASTElementExists          = handleWorkspaceCreation project mainBreadcrumbs
reactToBatchEvent project ASTElementDoesNotExist    = (createMain project, Nothing)
reactToBatchEvent project (GraphViewFetched graph)  = (print graph, Nothing)
reactToBatchEvent _       _                         = (return (), Nothing)

handleWorkspaceCreation :: Project -> Breadcrumbs -> Action
handleWorkspaceCreation project crumbs = (action, Just crumbs) where
    action = do
        setMain project crumbs
        fetchGraph project crumbs

fetchGraph :: Project -> Breadcrumbs -> IO ()
fetchGraph project crumbs = do
    let lib       = head $ project ^. libs
        workspace = Workspace project lib crumbs
    BatchCmd.getGraph workspace

setMain :: Project -> Breadcrumbs -> IO ()
setMain project crumbs = do
    let lib       = head $ project ^. libs
        workspace = Workspace project lib crumbs
    BatchCmd.setMainPtr workspace

mainBreadcrumbs :: Breadcrumbs
mainBreadcrumbs = Breadcrumbs [Module "Main", Function "main"]

createMain :: Project -> IO ()
createMain project = BatchCmd.createFunction project
                                             (head $ project ^. libs)
                                             (Breadcrumbs [Module "Main"])
                                             "main"

makeNetworkDescription :: forall t. Frameworks t => Project -> WebSocket -> (Workspace -> IO ()) -> Moment t ()
makeNetworkDescription project socket callback = do
    webSocketE <- fromAddHandler $ webSocketHandler socket

    let batchE    = filterJust $ process <$> webSocketE
        actions   = filterJust $ react project <$> batchE
        crumbs    = filterJust $ snd <$> actions
        workspace = Workspace project (head $ project ^. libs) <$> crumbs

    reactimate $ print    <$> workspace
    reactimate $ fst      <$> actions
    reactimate $ callback <$> workspace

run :: WebSocket -> (Workspace -> IO ()) -> Project -> IO ()
run socket callback project = do
    BatchCmd.setProjectId project
    BatchCmd.getAST project (head $ project ^. libs) mainBreadcrumbs
    net <- compile $ makeNetworkDescription project socket callback
    actuate net
