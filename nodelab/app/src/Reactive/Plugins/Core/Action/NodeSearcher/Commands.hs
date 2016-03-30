{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.NodeSearcher.Commands where

import qualified Data.IntMap.Lazy                 as IntMap
import           Data.Map                         (Map)
import qualified Data.Map                         as Map
import           Data.Text.Lazy                   (Text)
import qualified Data.Text.Lazy                   as Text
import           Utils.PreludePlus                hiding (Item)

import qualified Batch.Workspace                  as Workspace
import qualified BatchConnector.Commands          as BatchCmd
import           Empire.API.Data.NodeSearcher     (Item (..), LunaModule (..))
import qualified Empire.API.Data.Project          as Project
import           Reactive.Commands.Command        (Command, performIO)
import           Reactive.Commands.ProjectManager (loadProject)
import qualified Reactive.State.Global            as Global

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

foreign import javascript unsafe "_urq.push(['Feedback_Open'])" openFeedback' :: IO ()
foreign import javascript unsafe "$('.tutorial-box').show()"    openHelp' :: IO ()
