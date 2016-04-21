{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.NodeSearcher where

import           Data.Map                                        (Map)
import qualified Data.Map                                        as Map
import           Data.Text.Lazy                                  (Text, stripPrefix)
import qualified Data.Text.Lazy                                  as Text
import           Utils.PreludePlus                               hiding (stripPrefix)
import           Utils.Vector

import qualified JS.NodeSearcher                                 as UI

import qualified Batch.Workspace                                 as Workspace
import           Event.Event                                     (Event (..))
import           Event.Keyboard                                  (KeyMods (..))
import qualified Event.Keyboard                                  as Keyboard
import qualified Event.NodeSearcher                              as NodeSearcher
import qualified Object.Node                                     as Node
import qualified Object.Widget                                   as Widget
import qualified Object.Widget.Node                              as NodeModel

import           Reactive.Commands.Command                       (Command, performIO)
import qualified Reactive.Commands.CommandSearcher.Commands      as CS
import qualified Reactive.Commands.NodeSearcher                  as NS
import           Reactive.Commands.RegisterNode                  (registerNode)
import           Reactive.Commands.UpdateNode                    ()
import qualified Reactive.State.Global                           as Global
import qualified Reactive.State.Graph                            as Graph
import qualified Reactive.State.UIRegistry                       as UIRegistry

import           Empire.API.Data.NodeSearcher                    (Item (..))
import qualified Reactive.Plugins.Core.Action.NodeSearcher.Scope as Scope


toAction :: Event -> Maybe (Command Global.State ())
toAction (NodeSearcher (NodeSearcher.Event "query" expr _))           = Just $ NS.querySearch expr
toAction (NodeSearcher (NodeSearcher.Event "tree"  expr _))           = Just $ NS.queryTree expr
toAction (NodeSearcher (NodeSearcher.Event "create" expr Nothing))    = Just $ registerNode expr
-- toAction (NodeSearcher (NodeSearcher.Event "create" expr (Just nid))) = Just $ updateNode nid expr

toAction (NodeSearcher (NodeSearcher.Event "queryCmd" expr _))           = Just $ CS.querySearchCmd expr
toAction (NodeSearcher (NodeSearcher.Event "treeCmd"  expr _))           = Just $ CS.queryTreeCmd expr
toAction (NodeSearcher (NodeSearcher.Event "createCmd" expr Nothing))    = Just $ CS.runCommand expr

toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\t'   mods)) = Just $ NS.openFresh
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\191' (KeyMods False False False False))) = Just $ NS.openCommand -- 191 = /
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\191' (KeyMods True False False False))) = Just $ CS.help
toAction _ = Nothing

