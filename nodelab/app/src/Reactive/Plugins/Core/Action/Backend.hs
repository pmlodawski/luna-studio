module Reactive.Plugins.Core.Action.Backend where

import           Event.Event
import           Object.Node
import           Batch.Project
import qualified Event.Backend             as Backend
import           Utils.PreludePlus
import           Data.ByteString.Lazy      (ByteString)
import           BatchConnector.Updates
import           BatchConnector.Commands
import           BatchConnector.Connection

import           Reactive.Plugins.Core.Action.Action
import           Reactive.Plugins.Core.Action.State.Backend hiding (State)
import           Reactive.Plugins.Core.Action.State.Global

data Action = MessageAction { _msg    :: WebMessage }
            | OpenedAction
            | IOAction      { _action :: IO () }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "BackendAction"

toAction :: Event Node -> Maybe Action
toAction (Backend event) = Just $ case event of
    Backend.Message msg -> MessageAction msg
    Backend.Opened      -> OpenedAction
toAction _               = Nothing

instance ActionStateUpdater Action where
    execSt (MessageAction msg) st = handleMessage st msg
    execSt OpenedAction        st = ActionUI (IOAction $ putStrLn "Connection Opened!" >> sendMessage listProjects) (st & backend .~ AwaitingProject)

handleMessage :: State -> WebMessage -> ActionUI
handleMessage state (WebMessage topic bytes) = ActionUI (IOAction action) newState where
    (action, newState) = handler state bytes
    handler            = case (state ^. backend, topic) of
        (AwaitingProject, "project.list.status")         -> handleProjectsListResponse
        (AwaitingProject, "project.create.update")       -> handleProjectCreatedResponse
        (AwaitingLibs,    "project.library.list.status") -> handleLibrariesListResponse
        _                                                -> \st _ -> (print $ "Unexpected msg: " <> topic, st)

handleProjectsListResponse :: State -> ByteString -> (IO (), State)
handleProjectsListResponse state bytes = case parseProjectsList bytes of
    Nothing            -> die state
    Just []            -> (createFirstProject, state & backend .~ AwaitingProject)
    Just (project : _) -> startLibsFlow project state

handleProjectCreatedResponse :: State -> ByteString -> (IO (), State)
handleProjectCreatedResponse state bytes = case parseProjectCreateUpdate bytes of
    Nothing      -> die state
    Just project -> startLibsFlow project state

-- FIXME[Marcin Kostrzewa]: This whole `Maybe Project` in state is a terrible kludge, will disappear after redesigning
-- the reactive network and fetching projects before global state is initialized.
handleLibrariesListResponse :: State -> ByteString -> (IO (), State)
handleLibrariesListResponse state bytes = case parseLibrariesListResponse bytes of
    Nothing        -> die state
    Just []        -> (createFirstLibrary $ fromJust $ state ^. project, state)
    Just libraries -> (putStrLn "all is fine, watch my libs" >> print libraries, state & (backend .~ Ready))

die :: State -> (IO (), State)
die state = (putStrLn "Something went terribly wrong", state & backend .~ Fail)

startLibsFlow :: Project -> State -> (IO (), State)
startLibsFlow proj state = (sendMessage $ fetchLibraries proj, state & (backend .~ AwaitingLibs)
                                                                     . (project ?~ proj))

createFirstProject :: IO ()
createFirstProject  = do
    sendMessage $ createProject "myFirstProject" "some/path"
    putStrLn "Creating project!"

createFirstLibrary :: Project -> IO ()
createFirstLibrary  = sendMessage . (createLibrary "myFirstLib" "some/path")

instance ActionUIUpdater Action where
    updateUI (WithState (IOAction action) state) = action
