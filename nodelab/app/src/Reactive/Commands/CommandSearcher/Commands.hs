{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Reactive.Commands.CommandSearcher.Commands where

import qualified Data.IntMap.Lazy                                as IntMap
import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Data.Text.Lazy                                  (Text, stripPrefix)
import qualified Data.Text.Lazy                                  as Text
import           Utils.PreludePlus                               hiding (Item, stripPrefix)
import           Utils.Vector

import qualified Batch.Workspace                                 as Workspace
import qualified BatchConnector.Commands                         as BatchCmd
import           Empire.API.Data.NodeSearcher                    (Item (..), LunaModule (..))
import qualified Empire.API.Data.Project                         as Project
import qualified JS.NodeSearcher                                 as UI
import           Reactive.Commands.Command                       (Command, performIO)
import           Reactive.Commands.NodeSearcher                  as NS
import           Reactive.Commands.ProjectManager                (loadProject)
import qualified Reactive.Plugins.Core.Action.General            as General
import qualified Reactive.Plugins.Core.Action.NodeSearcher.Scope as Scope
import qualified Reactive.State.Camera                           as Camera
import qualified Reactive.State.Global                           as Global
import qualified Reactive.State.UIElements                       as UIElements

commands :: Command Global.State ([(Text, Item)])
commands = do
    projects <- uses (Global.workspace . Workspace.projects) IntMap.elems

    let projectToItem p = (name, Function) where name = Text.pack $ fromMaybe "no_name" $ p ^. Project.name
        projectList = LunaModule $ Map.fromList $ projectToItem <$> projects
        projectCmd  = LunaModule $ Map.fromList [ ("new",    Function)
                                                , ("open",   Module projectList)
                                                -- , ("rename", Function)
                                                ]
    return [ ("project",  Module projectCmd)
           , ("insert",   Function)
           , ("feedback", Function)
           , ("help",     Function)
           , ("toggleTextEditor",     Function)
           ]


createProject :: Text -> Command Global.State ()
createProject name = performIO $ BatchCmd.createProject name $ name <> ".luna"

openProject :: Text -> Command Global.State ()
openProject name = do
    projs <- use $ Global.workspace . Workspace.projects
    let mayProject = find (\(_,p) -> p ^. Project.name == (Just $ Text.unpack name)) (IntMap.toList projs)
    case mayProject of
        Just (projectId, project) -> loadProject projectId
        Nothing                   -> performIO $ putStrLn "Project not found"



help :: Command Global.State ()
help = performIO $ openHelp'

feedback :: Command Global.State ()
feedback = performIO $ openFeedback'

toggleText :: Command Global.State ()
toggleText = do
    Global.uiElements . UIElements.textEditorVisible %= not
    size <- use $ Global.camera . Camera.camera . Camera.windowSize
    General.updateWindowSize size

foreign import javascript unsafe "_urq.push(['Feedback_Open'])" openFeedback' :: IO ()
foreign import javascript unsafe "$('.tutorial-box').show().focus()"    openHelp' :: IO ()


runCommand :: Text -> Command Global.State ()
runCommand "project.new"                              = performIO $ UI.initNodeSearcher "project.new untitled" 0 (Vector2 200 200) True
runCommand (stripPrefix "project.new "  -> Just name) = createProject name
runCommand (stripPrefix "project.open." -> Just name) = openProject name
runCommand "help"                                     = help
runCommand "feedback"                                 = feedback
runCommand "insert"                                   = NS.openFresh
runCommand "toggleTextEditor"                         = toggleText
runCommand _                                          = return ()

querySearchCmd :: Text -> Command Global.State ()
querySearchCmd query = do
    sd <- commands
    let sd'   = LunaModule $ Map.fromList sd
        items = Scope.searchInScope True sd' query
    performIO $ UI.displayQueryResults items

queryTreeCmd :: Text -> Command Global.State ()
queryTreeCmd query = do
    sd <- commands
    let sd'   = LunaModule $ Map.fromList sd
        items = Scope.moduleItems True sd' query
    performIO $ UI.displayTreeResults items

