module Reactive.Commands.UILayout
    ( relayout
    , relayoutTextEditor
    ) where

import           Utils.PreludePlus
import           Utils.Vector                 (Vector2 (..), x, y)

import qualified JS.TextEditor                as UI
import           Object.Widget                (widgetSize)
import qualified Object.Widget.Button         as Button
import           Reactive.Commands.Command    (Command, performIO)
import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Camera        as Camera
import           Reactive.State.Global        (State, inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.UIElements    as UIElements
import qualified Style.Layout                 as Style


relayout :: Command State ()
relayout = do
    screenSize <- use $ Global.camera . Camera.camera . Camera.screenSize
    resizeSidebar screenSize
    resizeTextEditorToggle screenSize
    moveInputsEdge screenSize
    moveOutputsEdge screenSize

resizeSidebar :: Vector2 Int -> Command State ()
resizeSidebar screenSize = do
    sidebarId <- use $ Global.uiElements . UIElements.sidebar
    inRegistry $ UICmd.resize sidebarId (Vector2 Style.sidebarWidth $ fromIntegral $ screenSize ^. y)

    bcId <- use $ Global.uiElements . UIElements.breadcrumbs
    inRegistry $ UICmd.resize bcId (Vector2 (fromIntegral $ screenSize ^. x) Style.breadcrumbsHeight)

resizeTextEditorToggle :: Vector2 Int -> Command State ()
resizeTextEditorToggle screenSize = do
    toggleId <- use $ Global.uiElements . UIElements.textEditorToggle
    inRegistry $ do
        let width = Style.textEditorToggle ^. Button.size . x
        UICmd.moveX  toggleId $ (fromIntegral $ screenSize ^. x) - width
        UICmd.resize toggleId $ Vector2 width (fromIntegral $ screenSize ^. y)


relayoutTextEditor :: Vector2 Int -> Command Global.State Int
relayoutTextEditor screenSize = do
    visible <- use $ Global.uiElements . UIElements.textEditorVisible
    performIO $ UI.setVisible visible
    let width = (floor $ (0.3 :: Double) * (fromIntegral $ screenSize ^. x))
    performIO $ UI.setWidth width

    return $ if visible then width else 0


moveInputsEdge :: Vector2 Int -> Command Global.State ()
moveInputsEdge screenSize = do
    let height = fromIntegral $ screenSize ^. y
    inputsEdge <- use $ Global.uiElements . UIElements.inputsEdge
    inputsSize <- inRegistry $ UICmd.get' inputsEdge widgetSize
    inRegistry $ UICmd.moveY inputsEdge $ height / 2.0 - inputsSize ^. x / 2

moveOutputsEdge :: Vector2 Int -> Command Global.State ()
moveOutputsEdge screenSize = do
    let width  = fromIntegral $ screenSize ^. x
        height = fromIntegral $ screenSize ^. y
    outputsEdge <- use $ Global.uiElements . UIElements.outputsEdge
    outputsSize <- inRegistry $ UICmd.get' outputsEdge widgetSize

    inRegistry $ UICmd.moveX outputsEdge $ width - outputsSize ^. x
    inRegistry $ UICmd.moveY outputsEdge $ height / 2.0 - outputsSize ^. y / 2
