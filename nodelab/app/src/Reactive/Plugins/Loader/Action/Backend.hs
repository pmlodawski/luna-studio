module Reactive.Plugins.Loader.Action.Backend where

import           Data.Dynamic
import           Event.Event
import qualified Event.Backend             as Backend
import           Utils.PreludePlus
import           BatchConnector.Updates
import           BatchConnector.Commands
import           BatchConnector.Connection
import           Data.ByteString.Lazy      (ByteString)
import qualified Batch.Project             as Project

data ConnectionState = AwaitingConnection
                     | AwaitingProject
                     | AwaitingLibs
                     | Ready
                     | Fail
                     deriving (Eq, Show)

instance Default ConnectionState where
    def = AwaitingConnection

data State = State { _connection :: ConnectionState
                   , _project    :: Maybe Project.Project
                   } deriving (Eq, Show)

instance Default State where
    def = State def def

makeLenses ''State

type Action = (IO (), State)

makeReaction :: (State -> Action) -> Action -> Action
makeReaction f (_, state) = f state

react :: Event Dynamic -> Maybe (Action -> Action)
react (Backend event) = fmap makeReaction $ case event of
    Backend.Message msg -> Just $ reactToMessage msg
    Backend.Opened      -> Just $ reactToOpening
    _                   -> Nothing
react _               = Nothing

reactToMessage :: WebMessage -> State -> Action
reactToMessage (WebMessage topic bytes) state = handler state bytes where
    handler = case (state ^. connection, topic) of
        (AwaitingProject, "project.list.status")           -> handleProjectsListResponse
        (AwaitingProject, "project.create.update")         -> handleProjectCreatedResponse
        (AwaitingLibs,    "project.library.list.status")   -> handleLibrariesListResponse
        (AwaitingLibs,    "project.library.create.update") -> handleLibraryCreateResponse
        _                                                  -> \st _ -> (print $ "Unexpected msg: " <> topic, st)

reactToOpening :: State -> Action
reactToOpening state = (sendMessage listProjects, state & connection .~ AwaitingProject)

handleProjectsListResponse :: State -> ByteString -> Action
handleProjectsListResponse state bytes = case parseProjectsList bytes of
    Nothing            -> die state
    Just []            -> (createFirstProject, state & connection .~ AwaitingProject)
    Just (project : _) -> startLibsFlow project state

handleProjectCreatedResponse :: State -> ByteString -> Action
handleProjectCreatedResponse state bytes = case parseProjectCreateUpdate bytes of
    Nothing      -> die state
    Just project -> startLibsFlow project state

handleLibrariesListResponse state bytes = case (libs, currentProject) of
    (Nothing,        _)         -> die state
    (_,              Nothing)   -> die state
    (Just [],        Just proj) -> (createFirstLibrary proj, state)
    (Just libraries, Just proj) -> (return (), state & (connection .~ Ready)
                                                     . (project    ?~ (proj & Project.libs .~ libraries)))
    where
        libs           = parseLibrariesListResponse bytes
        currentProject = state ^. project

handleLibraryCreateResponse :: State -> ByteString -> Action
handleLibraryCreateResponse state bytes = case (lib, currentProject) of
    (Nothing,      _)         -> die state
    (_,            Nothing)   -> die state
    (Just library, Just proj) -> (return (), state & (connection .~ Ready)
                                                   . (project    ?~ (proj & Project.libs .~ [library])))
    where
        lib            = parseLibraryCreateResponse bytes
        currentProject = state ^. project

startLibsFlow :: Project.Project -> State -> Action
startLibsFlow proj state = (sendMessage $ fetchLibraries proj, state & (connection .~ AwaitingLibs)
                                                                     . (project    ?~ proj))

createFirstProject :: IO ()
createFirstProject  = do
    putStrLn "Creating project!"
    sendMessage $ createProject "myFirstProject" "some/path"

createFirstLibrary :: Project.Project -> IO ()
createFirstLibrary  = sendMessage . (createLibrary "myFirstLib" "some/path")

die :: State -> Action
die state = (putStrLn "Something went terribly wrong", state & connection .~ Fail)
