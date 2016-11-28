{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.Graph.Unrender
    ( unrender
    ) where

import           Utils.PreludePlus

import           Reactive.Commands.Command    (Command, performIO)
import           Reactive.State.Global        (State, inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.Graph         as Graph

import qualified Batch.Workspace              as Workspace

import qualified JS.TextEditor                as UI
import           Reactive.Commands.UIRegistry (removeWidget)
import           UI.Instances                 ()



unrender :: Command State ()
unrender = do
    nodeWidgets   <- use $ Global.graph . Graph.nodeWidgets
    connWidgets   <- use $ Global.graph . Graph.connectionWidgets

    let allWidgetIds = nodeWidgets ++ connWidgets
    inRegistry $ mapM_ removeWidget allWidgetIds

    Global.graph     . Graph.nodeWidgetsMap       .= def
    Global.graph     . Graph.connectionWidgetsMap .= def
    Global.workspace . Workspace.isGraphLoaded    .= False

    performIO $ UI.setText ""

    Global.graph .= def
