{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Node.Create
    ( addNode
    , addDummyNode
    , registerNode
    ) where

import           Utils.PreludePlus
import           Utils.Vector

import           Control.Monad.State               (modify)
import qualified Data.Text.Lazy                    as Text

import           Object.UITypes                    (WidgetId)
import           Object.Widget                     (objectId, widget)
import qualified Object.Widget.Node                as Model
import qualified UI.Handlers.Node                  as Node

import           Reactive.Commands.Command         (Command)
import           Reactive.Commands.EnterNode       (enterNode)
import           Reactive.Commands.Graph           (focusNode)
import           Reactive.Commands.Graph.Selection (selectedNodes)
import           Reactive.Commands.Node.Remove     (removeSelectedNodes)
import qualified Reactive.Commands.UIRegistry      as UICmd
import           Reactive.State.Global             (State, inRegistry)
import qualified Reactive.State.Global             as Global
import qualified Reactive.State.Graph              as Graph
import           Reactive.State.UIRegistry         (addHandler, sceneGraphId)

import qualified Reactive.Commands.Batch           as BatchCmd

import           Data.HMap.Lazy                    (HTMap)
import qualified UI.Handlers.Node                  as UINode

import qualified Empire.API.Data.Breadcrumb        as Breadcrumb
import           Empire.API.Data.Node              (Node, NodeId)
import qualified Empire.API.Data.Node              as Node
import qualified Empire.API.Data.NodeMeta          as NodeMeta

import           Reactive.Commands.Node.Ports      (displayPorts)


addNode :: Node -> Command State ()
addNode node = do
    zoom Global.graph $ modify (Graph.addNode node)

    widgetId <- registerNode node
    focusNode widgetId
    Node.selectNode' Node.performSelect widgetId

addDummyNode :: Node -> Command State ()
addDummyNode dummyNode = do
    mayNode <- preuse $ Global.graph . Graph.nodesMap . ix (dummyNode ^. Node.nodeId)
    case mayNode of
        Just _  -> return ()
        Nothing -> addNode dummyNode


registerNode :: Node -> Command State WidgetId
registerNode node = do
    let nodeModel = Model.fromNode node
        nodeId    = node ^. Node.nodeId
    nodeWidget <- inRegistry $ UICmd.register sceneGraphId nodeModel (nodeHandlers node)
    Global.graph . Graph.nodeWidgetsMap . at nodeId ?= nodeWidget
    displayPorts nodeWidget node
    return nodeWidget

nodeHandlers :: Node -> HTMap
nodeHandlers node = addHandler (UINode.RemoveNodeHandler removeSelectedNodes)
                  $ addHandler (UINode.RenameNodeHandler $ \_ nodeId name -> BatchCmd.renameNode nodeId name)
                  $ addHandler (UINode.ChangeInputNodeTypeHandler $ \_ nodeId name -> BatchCmd.setInputNodeType nodeId name)
                  $ addHandler (UINode.FocusNodeHandler    $ focusNode)
                  $ addHandler (UINode.ExpandNodeHandler   $ expandSelectedNodes)
                  $ addHandler (UINode.NodeRequiredHandler $ \nid val -> updateNodeRequired nid val)
                  $ addEnterNodeHandler where
                        addEnterNodeHandler = case node ^. Node.nodeType of
                            Node.FunctionNode _ -> addHandler (UINode.EnterNodeHandler $ enterNode $ Breadcrumb.Function $ Text.unpack $ node ^. Node.name) mempty
                            Node.ModuleNode     -> addHandler (UINode.EnterNodeHandler $ enterNode $ Breadcrumb.Module   $ Text.unpack $ node ^. Node.name) mempty
                            _                   -> mempty

expandSelectedNodes :: Command Global.State ()
expandSelectedNodes = do
    sn <- selectedNodes
    let allSelected = all (view $ widget . Model.isExpanded) sn
        update      = if allSelected then Model.isExpanded %~ not
                                     else Model.isExpanded .~ True
    inRegistry $ forM_ sn $ \wf -> do
        let id = wf ^. objectId
        UICmd.update_ id update
        UICmd.moveBy  id (Vector2 0 0) -- FIXME: trigger moved handler for html widgets

updateNodeRequired :: NodeId -> Bool -> Command State ()
updateNodeRequired id val = do
    Global.graph . Graph.nodesMap . ix id . Node.nodeMeta . NodeMeta.isRequired .= val
    newMeta <- preuse $ Global.graph . Graph.nodesMap . ix id . Node.nodeMeta
    withJust newMeta $ \newMeta -> BatchCmd.updateNodeMeta id newMeta
