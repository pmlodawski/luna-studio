{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Graph.Unrender
    ( unrender
    ) where

import           Utils.PreludePlus

import           Reactive.Commands.Command    (Command, performIO)
import           Reactive.State.Global        (State, inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.Graph         as Graph
import qualified Reactive.State.UIRegistry    as UIRegistry

import qualified Batch.Workspace              as Workspace

import qualified JS.TextEditor                as UI
import           Object.Widget                (WidgetFile, objectId)
import           Object.Widget.Connection     (Connection)
import           Object.Widget.Node           (Node)
import           Reactive.Commands.UIRegistry (removeWidget)
import           UI.Instances                 ()

unrender :: Command State ()
unrender = do
    Global.graph .= def
    uiRegistry <- use Global.uiRegistry
    nodeWidgets <- use $ Global.graph . Graph.nodeWidgets
    connWidgets <- use $ Global.graph . Graph.connectionWidgets
    let allWidgetIds = nodeWidgets ++ connWidgets

    inRegistry $ mapM_ removeWidget allWidgetIds

    Global.graph     . Graph.nodeWidgetsMap       .= def
    Global.graph     . Graph.connectionWidgetsMap .= def
    Global.workspace . Workspace.isGraphLoaded    .= False

    performIO $ UI.setText ""
