{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Reactive.Plugins.Core.Action.NodeSearcher where

import           Data.Map                                           (Map)
import qualified Data.Map                                           as Map
import           Data.Text.Lazy                                     (Text, stripPrefix)
import qualified Data.Text.Lazy                                     as Text
import           Utils.PreludePlus                                  hiding (stripPrefix)
import           Utils.Vector

import qualified JS.NodeSearcher                                    as UI

import           Event.Event                                        (Event (..))
import qualified Batch.Workspace                                    as Workspace
import qualified Event.Keyboard                                     as Keyboard
import qualified Event.NodeSearcher                                 as NodeSearcher
import qualified Object.Node                                        as Node
import qualified Object.Widget                                      as Widget
import qualified Object.Widget.Node                                 as NodeModel

import           Reactive.Commands.Command                          (Command, performIO)
import           Reactive.Commands.RegisterNode                     (registerNode)
import           Reactive.Commands.UpdateNode                       ()
import qualified Reactive.Plugins.Core.Action.NodeSearcher.Commands as Commands
import qualified Reactive.State.Global                              as Global
import qualified Reactive.State.Graph                               as Graph
import qualified Reactive.State.UIRegistry                          as UIRegistry

import qualified Reactive.Plugins.Core.Action.NodeSearcher.Scope    as Scope
import           Empire.API.Data.NodeSearcher (LunaModule(..), Item(..))


toAction :: Event -> Maybe (Command Global.State ())
toAction (NodeSearcher (NodeSearcher.Event "query" expr _))           = Just $ querySearch expr
toAction (NodeSearcher (NodeSearcher.Event "tree"  expr _))           = Just $ queryTree expr
toAction (NodeSearcher (NodeSearcher.Event "create" expr Nothing))    = Just $ registerNode expr
-- toAction (NodeSearcher (NodeSearcher.Event "create" expr (Just nid))) = Just $ updateNode nid expr

toAction (NodeSearcher (NodeSearcher.Event "queryCmd" expr _))           = Just $ querySearchCmd expr
toAction (NodeSearcher (NodeSearcher.Event "treeCmd"  expr _))           = Just $ queryTreeCmd expr
toAction (NodeSearcher (NodeSearcher.Event "createCmd" expr Nothing))    = Just $ parseExpr expr

toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\t'   mods)) = Just $ openFresh
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\191' mods)) = Just $ openCommand -- 191 = /
toAction _ = Nothing

searcherData :: Command Global.State LunaModule
searcherData = use $ Global.workspace . Workspace.nodeSearcherData

openFresh :: Command Global.State ()
openFresh = do
    mousePos <- use Global.mousePos
    performIO $ UI.initNodeSearcher "" 0 mousePos False

querySearch :: Text -> Command Global.State ()
querySearch query = do
    sd <- searcherData
    let items = Scope.searchInScope sd query
    performIO $ UI.displayQueryResults items

queryTree :: Text -> Command Global.State ()
queryTree query = do
    sd <- searcherData
    let items = Scope.moduleItems sd query
    performIO $ UI.displayTreeResults items

openCommand :: Command Global.State ()
openCommand = do
    mousePos <- use Global.mousePos
    performIO $ UI.initNodeSearcher "" 0 mousePos True

querySearchCmd :: Text -> Command Global.State ()
querySearchCmd query = do
    sd <- Commands.commands
    let sd'   = LunaModule $ Map.fromList sd
        items = Scope.searchInScope sd' query
    performIO $ UI.displayQueryResults items

queryTreeCmd :: Text -> Command Global.State ()
queryTreeCmd query = do
    sd <- Commands.commands
    let sd'   = LunaModule $ Map.fromList sd
        items = Scope.moduleItems sd' query
    performIO $ UI.displayTreeResults items

parseExpr :: Text -> Command Global.State ()
parseExpr "project.new" = performIO $ UI.initNodeSearcher "project.new untitled" 0 (Vector2 200 200) True
parseExpr (stripPrefix "project.new "  -> Just name) = Commands.createProject name
parseExpr (stripPrefix "project.open." -> Just name) = Commands.openProject name
parseExpr "help"     = Commands.help
parseExpr "feedback" = Commands.feedback
parseExpr _ = return ()
