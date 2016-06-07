{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.NodeSearcher where

import           Utils.PreludePlus                               hiding (stripPrefix)

import           Event.Event                                     (Event (..))
import           Event.Keyboard                                  (KeyMods (..))
import qualified Event.Keyboard                                  as Keyboard
import qualified Event.NodeSearcher                              as NodeSearcher

import           Reactive.Commands.Command                       (Command)
import qualified Reactive.Commands.CommandSearcher.Commands      as CS
import qualified Reactive.Commands.NodeSearcher                  as NS
import           Reactive.Commands.RegisterNode                  (registerNode)
import qualified Reactive.State.Global                           as Global


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

