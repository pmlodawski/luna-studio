{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.NodeSearcher.Commands where

import qualified Data.IntMap.Lazy                                as IntMap
import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Data.Text.Lazy                                  (Text)
import qualified Data.Text.Lazy                                  as Text
import           Utils.PreludePlus                               hiding (Item)

import qualified Batch.Workspace                                 as Workspace
import qualified Empire.API.Data.Project                         as Project
import           Reactive.Commands.Command                       (Command)
import           Reactive.Plugins.Core.Action.NodeSearcher.Scope (Item (..), LunaModule (..))
import qualified Reactive.State.Global                           as Global

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
           , ("feedback", Function)
           , ("help",     Function)
           ]
