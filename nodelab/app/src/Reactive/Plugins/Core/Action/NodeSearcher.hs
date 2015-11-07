{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.NodeSearcher where

import           Utils.PreludePlus

import qualified JS.NodeSearcher    as UI

import qualified Object.Widget      as Widget
import qualified Object.Node        as Node
import qualified Object.Widget.Node as NodeModel
import qualified Event.Keyboard     as Keyboard
import qualified Event.NodeSearcher as NodeSearcher
import           Event.Event        (Event(..))

import qualified Reactive.State.Global          as Global
import qualified Reactive.State.Graph           as Graph
import qualified Reactive.State.UIRegistry      as UIRegistry
import           Reactive.Commands.Command      (Command, performIO)
import           Reactive.Commands.RegisterNode (registerNode)
import           Reactive.Commands.UpdateNode   (updateNode)

import qualified Reactive.Plugins.Core.Action.NodeSearcher.Mock as Mock
import           Data.Text.Lazy (Text)

toAction :: Event -> Maybe (Command Global.State ())
toAction (NodeSearcher (NodeSearcher.Event "query" expr _))           = Just $ querySearch expr
toAction (NodeSearcher (NodeSearcher.Event "tree"  expr _))           = Just $ queryTree expr
toAction (NodeSearcher (NodeSearcher.Event "create" expr Nothing))    = Just $ registerNode expr
toAction (NodeSearcher (NodeSearcher.Event "create" expr (Just nid))) = Just $ updateNode nid expr
toAction (Keyboard (Keyboard.Event Keyboard.Down '\t' mods))          = Just $ if mods ^. Keyboard.shift
    then openEdit
    else openFresh
toAction _ = Nothing

querySearch :: Text -> Command a ()
querySearch = performIO . UI.displayQueryResults . Mock.getItemsSearch

queryTree :: Text -> Command a ()
queryTree = performIO . UI.displayTreeResults . Mock.getItemsTree

openFresh :: Command Global.State ()
openFresh = do
    mousePos <- use Global.mousePos
    performIO $ UI.initNodeSearcher "" 0 mousePos

openEdit :: Command Global.State ()
openEdit = do
    focusedWidget  <- use $ Global.uiRegistry . UIRegistry.focusedWidget
    mousePos       <- use Global.mousePos
    graph          <- use Global.graph
    forM_ focusedWidget $ \focusedWidget -> do
        nodeWidget <- zoom Global.uiRegistry $ UIRegistry.lookupTypedM focusedWidget
        forM_ nodeWidget $ \nodeWidget -> do
            performIO $ UI.initNodeSearcher (nodeWidget ^. Widget.widget . NodeModel.expression)
                                            (nodeWidget ^. Widget.widget . NodeModel.nodeId)
                                            mousePos
