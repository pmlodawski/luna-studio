{-# LANGUAGE OverloadedStrings #-}
module Reactive.Commands.UnrenderGraph
    ( unrender
    ) where

import           Reactive.Commands.Command    (Command, performIO)
import           Reactive.State.Global        (State, inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.UIRegistry    as UIRegistry
import           Utils.PreludePlus

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
    let nodeWidgets  = UIRegistry.lookupAll uiRegistry :: [WidgetFile Node]
        connWidgets  = UIRegistry.lookupAll uiRegistry :: [WidgetFile Connection]
        allWidgetIds = (view objectId <$> nodeWidgets) ++ (view objectId <$> connWidgets)

    inRegistry $ mapM_ removeWidget allWidgetIds

    Global.workspace . Workspace.isGraphLoaded .= False

    performIO $ UI.setText ""
