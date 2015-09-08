module Reactive.Plugins.Loader.ProjectManager.Actions where

import           Utils.PreludePlus
import           Data.Dynamic
import           Data.ByteString.Lazy      (ByteString)

import qualified Event.Event               as Event
import qualified Event.Connection          as Connection
import           Event.Batch               as Batch

import           BatchConnector.Commands   as BatchCmd
import           Batch.Project
import           Batch.Library

import           Reactive.Plugins.Loader.ProjectManager.State

type Action = (IO (), State)

makeReaction :: (State -> Action) -> Action -> Action
makeReaction f (_, state) = f state

react :: Event.Event Dynamic -> Maybe (Action -> Action)
react event = makeReaction <$> handler event where
    handler (Event.Connection Connection.Opened) = Just $ reactToOpening
    handler (Event.Batch event)                  = Just $ reactToBatchEvent event
    handler _                                    = Nothing

reactToOpening :: State -> Action
reactToOpening state = (BatchCmd.listProjects, AwaitingProject)

reactToBatchEvent :: Batch.Event -> State -> Action
reactToBatchEvent event state = handler state where
    handler = case (state, event) of
        (AwaitingProject,   ProjectsList projects)  -> handleProjectsListResponse projects
        (AwaitingProject,   ProjectCreated project) -> handleProjectCreatedResponse project
        (AwaitingLibs proj, LibrariesList libs)     -> handleLibrariesListResponse libs proj
        (AwaitingLibs proj, LibraryCreated lib)     -> handleLibraryCreatedResponse lib proj
        (Ready _,           _)                      -> \st -> (return (), AfterInitialize)
        _                                           -> \st -> (return (), st)

handleProjectsListResponse :: [Project] -> State -> Action
handleProjectsListResponse []            state = (createFirstProject, state)
handleProjectsListResponse (project : _) state = startLibsFlow project state

handleProjectCreatedResponse :: Project -> State -> Action
handleProjectCreatedResponse = startLibsFlow

handleLibrariesListResponse :: [Library] -> Project -> State -> Action
handleLibrariesListResponse []        proj state = (createFirstLibrary proj, state)
handleLibrariesListResponse libraries proj state = (return (), Ready $ (proj & libs .~ libraries))

handleLibraryCreatedResponse :: Library -> Project -> State -> Action
handleLibraryCreatedResponse lib proj state = (return (), Ready $ (proj & libs .~ [lib]))

startLibsFlow :: Project -> State -> Action
startLibsFlow project state = (BatchCmd.fetchLibraries project, AwaitingLibs project)

createFirstProject :: IO ()
createFirstProject  = BatchCmd.createProject "myFirstProject" "some/path"

createFirstLibrary :: Project -> IO ()
createFirstLibrary project = BatchCmd.createLibrary "Main" "some/path" project
