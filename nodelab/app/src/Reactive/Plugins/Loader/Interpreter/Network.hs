module Reactive.Plugins.Loader.Interpreter.Network where

import Utils.PreludePlus

import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Handlers

import           Data.Dynamic
import           Data.ByteString.Lazy    (ByteString)
import           JS.WebSocket
import           JS.Bindings             (displayRejectedMessage)
import qualified Event.Event             as Event
import           Event.Batch             as Batch
import           Event.Processors.Batch  (process)
import qualified BatchConnector.Commands as BatchCmd
import           Batch.Project           as Project
import           Batch.Breadcrumbs
import           BatchConnector.Updates
import           Batch.Workspace
import           Batch.Breadcrumbs
import           Data.Int

data State = AwaitingWorkspace
           | InterpreterSetup Workspace
           | Ready Workspace
           | AfterInitialize

type Action = (IO (), State)

makeReaction :: (State -> Action) -> Action -> Action
makeReaction f (_, state) = f state

react :: Project -> Event.Event Dynamic -> Maybe (Action -> Action)
react project event = makeReaction <$> handler event where
    handler (Event.Batch event) = Just $ reactToBatchEvent project event
    handler _                   = Nothing

reactToBatchEvent :: Project -> Batch.Event -> State -> Action
reactToBatchEvent project event state = case (state, event) of
    (AwaitingWorkspace,          WorkspaceCreated crumbs)    -> handleWorkspaceCreation project crumbs
    (AwaitingWorkspace,          ASTElementExists)           -> handleWorkspaceCreation project mainBreadcrumbs
    (AwaitingWorkspace,          ASTElementDoesNotExist)     -> (createMain project, AwaitingWorkspace)
    (InterpreterSetup workspace, InterpreterGotProjectId id) -> handleProjectIdResponse workspace id
    (Ready _,                    _)                          -> (return (), AfterInitialize)
    (state,                      ConnectionDropped)          -> (displayRejectedMessage, AfterInitialize)
    (state,                      _)                          -> (return (), state)

handleWorkspaceCreation :: Project -> Breadcrumbs -> Action
handleWorkspaceCreation project crumbs = (action, InterpreterSetup workspace) where
    action    = BatchCmd.getProjectId
    workspace = Workspace project (head $ project ^. libs) crumbs Fresh False

handleProjectIdResponse :: Workspace -> Maybe Int32 -> Action
handleProjectIdResponse workspace Nothing = setupInterpreterAction workspace
handleProjectIdResponse workspace (Just id)
    | id == (workspace ^. project . Project.id) = (return (), Ready $ workspace & interpreterState .~ AllSet)
    | otherwise                                 = setupInterpreterAction workspace

setupInterpreterAction :: Workspace -> Action
setupInterpreterAction workspace = (setupInterpreter workspace, Ready $ workspace & interpreterState .~ Fresh)

setupInterpreter :: Workspace -> IO ()
setupInterpreter workspace =  BatchCmd.setProjectId (workspace ^. project)
                           >> BatchCmd.setMainPtr workspace

mainBreadcrumbs :: Breadcrumbs
mainBreadcrumbs = Breadcrumbs [Module "Main", Function "main"]

createMain :: Project -> IO ()
createMain project = do
    let lib   = head $ project ^. libs
        crumb = Breadcrumbs [Module "Main"]
    BatchCmd.createFunction project lib (Breadcrumbs [Module "Main"]) "main"
    BatchCmd.setImport project lib crumb ["Flowbox"] "Std"

readyWorkspace :: State -> Maybe Workspace
readyWorkspace (Ready workspace) = Just workspace
readyWorkspace _                 = Nothing

makeNetworkDescription :: forall t. Frameworks t => Project -> WebSocket -> (Workspace -> IO ()) -> Moment t ()
makeNetworkDescription project socket callback = do
    webSocketE <- fromAddHandler $ webSocketHandler socket

    let batchE    = filterJust $ process <$> webSocketE
        updates   = filterJust $ react project <$> batchE
        actions   = accumE (return (), AwaitingWorkspace) updates
        states    = snd <$> actions
        workspace = filterJust $ readyWorkspace <$> states

    reactimate $ print    <$> workspace
    reactimate $ fst      <$> actions
    reactimate $ callback <$> workspace

run :: WebSocket -> (Workspace -> IO ()) -> Project -> IO ()
run socket callback project = do
    BatchCmd.getAST project (head $ project ^. libs) mainBreadcrumbs
    net <- compile $ makeNetworkDescription project socket callback
    actuate net
