module Reactive.Plugins.Loader.ProjectManager.Actions where

import           Utils.PreludePlus
import           Data.Dynamic
import           Data.ByteString.Lazy      (ByteString)
import           JS.Bindings               (displayRejectedMessage)

import qualified Event.Event               as Event
import qualified Event.Connection          as Connection
import           Event.Batch               as Batch

import qualified BatchConnector.Commands   as BatchCmd
import           Batch.Project             as Project
import           Batch.Library

import           Reactive.Plugins.Loader.ProjectManager.State

type Action = (IO (), State)

makeReaction :: (State -> Action) -> Action -> Action
makeReaction f (_, state) = f state

react :: Event.Event Dynamic -> Maybe (Action -> Action)
react event = makeReaction <$> handler event where
    handler (Event.Batch event)                  = Just $ reactToBatchEvent event
    handler _                                    = Nothing

reactToBatchEvent :: Batch.Event -> State -> Action
reactToBatchEvent event state = handler state where
    handler = case (state, event) of
        (AwaitingProject name, ConnectionOpened)           -> handleOpening
        (AwaitingProject name, ProjectsList projects)      -> handleProjectsListResponse projects name
        (AwaitingProject name, ProjectCreated project)     -> handleProjectCreatedResponse project
        (AwaitingLibs proj,    LibrariesList libs)         -> handleLibrariesListResponse libs proj
        (AwaitingLibs proj,    LibraryCreated lib)         -> handleLibraryCreatedResponse lib proj
        (Ready _,              _)                          -> \st -> (return (), AfterInitialize)
        (_,                    DuplicateConnectionRefused) -> \st -> (displayRejectedMessage, AfterInitialize)
        _                                                  -> \st -> (return (), st)

handleOpening :: State -> Action
handleOpening state = (BatchCmd.listProjects, state)

handleProjectsListResponse :: [Project] -> String -> State -> Action
handleProjectsListResponse []       name state = (createProject name, state)
handleProjectsListResponse projects name state = case findProjectByName projects name of
    Nothing        -> (createProject name, state)
    (Just project) -> startLibsFlow project

findProjectByName :: [Project] -> String -> Maybe Project
findProjectByName projects name = find (\project -> Just name == project ^. Project.name) projects

handleProjectCreatedResponse :: Project -> State -> Action
handleProjectCreatedResponse project _ = startLibsFlow project

handleLibrariesListResponse :: [Library] -> Project -> State -> Action
handleLibrariesListResponse []        proj state = (createFirstLibrary proj, state)
handleLibrariesListResponse libraries proj state = (return (), Ready $ (proj & libs .~ libraries))

handleLibraryCreatedResponse :: Library -> Project -> State -> Action
handleLibraryCreatedResponse lib proj state = (return (), Ready $ (proj & libs .~ [lib]))

startLibsFlow :: Project -> Action
startLibsFlow project = (BatchCmd.fetchLibraries project, AwaitingLibs project)

createProject :: String -> IO ()
createProject name = BatchCmd.createProject name $ "projects/" ++ name

createFirstLibrary :: Project -> IO ()
createFirstLibrary project = BatchCmd.createLibrary "Main" "some/path" project
