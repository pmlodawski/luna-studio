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
import qualified Object.Widget                                   as Widget
import qualified Object.Widget.Node                              as NodeModel

import           Reactive.Commands.Command                       (Command, performIO)
import qualified Reactive.Commands.CommandSearcher.Commands      as CS
import qualified Reactive.Commands.NodeSearcher                  as NS
import           Reactive.Commands.RegisterNode                  (registerNode)
import qualified Reactive.State.Global                           as Global
import qualified Reactive.State.Graph                            as Graph
import qualified Reactive.State.UIRegistry                       as UIRegistry


toAction :: Event -> Maybe (Command Global.State ())
toAction (NodeSearcher (NodeSearcher.Query  expr)) = Just $ NS.querySearch expr
toAction (NodeSearcher (NodeSearcher.Tree   expr)) = Just $ NS.queryTree expr
toAction (NodeSearcher (NodeSearcher.Create expr)) = Just $ registerNode expr

toAction (NodeSearcher (NodeSearcher.QueryCmd  expr)) = Just $ CS.querySearchCmd expr
toAction (NodeSearcher (NodeSearcher.TreeCmd   expr)) = Just $ CS.queryTreeCmd expr
toAction (NodeSearcher (NodeSearcher.CreateCmd expr)) = Just $ CS.runCommand expr

toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\t'   mods)) = Just $ NS.openFresh
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\191' (KeyMods False False False False))) = Just $ NS.openCommand -- 191 = /
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\191' (KeyMods True False False False))) = Just $ CS.help
toAction _ = Nothing

