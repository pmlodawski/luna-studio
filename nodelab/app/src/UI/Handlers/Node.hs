{-# LANGUAGE OverloadedStrings #-}
module UI.Handlers.Node where

import           Utils.PreludePlus

import           Data.HMap.Lazy               (HTMap, TypeKey (..))
import qualified Data.HMap.Lazy               as HMap
import qualified Data.Text.Lazy               as Text
import           Data.Text.Lazy.Read          (rational)
import           Utils.Vector

import           Event.Event                  (JSState)
import           Event.Keyboard               (KeyMods (..))
import           Event.Mouse                  (MouseButton (..))
import qualified Event.Mouse                  as Mouse
import           Object.Widget                (CompositeWidget, DblClickHandler, DragEndHandler, DragMoveHandler,
                                               KeyPressedHandler, KeyUpHandler, MousePressedHandler, ResizableWidget,
                                               UIHandlers, WidgetFile, WidgetId, click, createWidget, currentPos,
                                               dblClick, dragEnd, dragMove, keyDown, keyMods, keyUp, mousePressed,
                                               objectId, resizeWidget, startPos, updateWidget)

import qualified Object.Widget.Group          as Group
import qualified Object.Widget.Label          as Label
import qualified Object.Widget.Node           as Model
import qualified Object.Widget.TextBox        as TextBox
import qualified Object.Widget.LabeledTextBox as LabeledTextBox
import           Reactive.Commands.Command    (Command, performIO)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global
import           Reactive.State.UIRegistry    (addHandler)
import qualified Reactive.State.UIRegistry    as UIRegistry

import           UI.Generic                   (defaultResize, startDrag)
import           UI.Handlers.Generic          (triggerValueChanged)
import           UI.Handlers.Button           (MousePressedHandler(..))
import           UI.Handlers.Generic          (ValueChangedHandler (..), triggerValueChanged)
import           UI.Layout                    as Layout
import           UI.Widget.Group              ()
import           UI.Widget.Label              ()
import           UI.Widget.Node               ()
import           UI.Widget.TextBox            ()
import           UI.Widget.LabeledTextBox     ()
import           UI.Handlers.LabeledTextBox   ()
import qualified Style.Node as Style

import           Empire.API.Data.Node         (NodeId)


nameHandlers :: WidgetId -> HTMap
nameHandlers id = addHandler (ValueChangedHandler $ nameValueChangedHandler id)
                $ addHandler (UICmd.LostFocus $ inRegistry . flip UICmd.update_ (TextBox.isEditing .~ False))
                $ mempty where

nameValueChangedHandler :: WidgetId -> Text -> WidgetId -> Command Global.State ()
nameValueChangedHandler parent val tbId = do
    model <- inRegistry $ UICmd.update parent $ Model.name .~ val
    triggerRenameNodeHandler parent model

typeHandlers :: WidgetId -> HTMap
typeHandlers id = addHandler (ValueChangedHandler $ typeValueChangedHandler id)
                $ addHandler (UICmd.LostFocus $ inRegistry . flip UICmd.update_ (TextBox.isEditing .~ False))
                $ mempty where

typeValueChangedHandler :: WidgetId -> Text -> WidgetId -> Command Global.State ()
typeValueChangedHandler parent val tbId = do
    model <- inRegistry $ UICmd.update parent $ Model.tpe ?~ val
    triggerChangeInputNodeTypeHandler parent model

newtype RemoveNodeHandler = RemoveNodeHandler (Command Global.State ())
removeNodeHandler = TypeKey :: TypeKey RemoveNodeHandler

newtype FocusNodeHandler = FocusNodeHandler (WidgetId -> Command Global.State ())
focusNodeHandler = TypeKey :: TypeKey FocusNodeHandler

newtype RenameNodeHandler = RenameNodeHandler (WidgetId -> NodeId -> Text -> Command Global.State ())
renameNodeHandler = TypeKey :: TypeKey RenameNodeHandler

newtype ChangeInputNodeTypeHandler = ChangeInputNodeTypeHandler (WidgetId -> NodeId -> Text -> Command Global.State ())
changeInputNodeTypeHandler = TypeKey :: TypeKey ChangeInputNodeTypeHandler

newtype EnterNodeHandler = EnterNodeHandler (Command Global.State ())
enterNodeHandler = TypeKey :: TypeKey EnterNodeHandler

triggerRemoveHandler :: WidgetId -> Command Global.State ()
triggerRemoveHandler id = do
    maybeHandler <- inRegistry $ UICmd.handler id removeNodeHandler
    forM_ maybeHandler $ \(RemoveNodeHandler handler) -> handler

triggerFocusNodeHandler :: WidgetId -> Command Global.State ()
triggerFocusNodeHandler id = do
    maybeHandler <- inRegistry $ UICmd.handler id focusNodeHandler
    forM_ maybeHandler $ \(FocusNodeHandler handler) -> handler id

triggerRenameNodeHandler :: WidgetId -> Model.Node -> Command Global.State ()
triggerRenameNodeHandler id model = do
    maybeHandler <- inRegistry $ UICmd.handler id renameNodeHandler
    forM_ maybeHandler $ \(RenameNodeHandler handler) -> handler id (model ^. Model.nodeId) (model ^. Model.name)

triggerChangeInputNodeTypeHandler :: WidgetId -> Model.Node -> Command Global.State ()
triggerChangeInputNodeTypeHandler id model = do
    withJust (model ^. Model.tpe) $ \tpe -> do
        maybeHandler <- inRegistry $ UICmd.handler id changeInputNodeTypeHandler
        forM_ maybeHandler $ \(ChangeInputNodeTypeHandler handler) -> handler id (model ^. Model.nodeId) tpe

triggerEnterNodeHandler :: WidgetId -> Command Global.State ()
triggerEnterNodeHandler id = do
    maybeHandler <- inRegistry $ UICmd.handler id enterNodeHandler
    forM_ maybeHandler $ \(EnterNodeHandler handler) -> handler

keyDownHandler :: KeyPressedHandler Global.State
keyDownHandler '\r'   _ _ id = void $ inRegistry $ do
    UICmd.update_ id (Model.isExpanded %~ not)
    UICmd.moveBy  id (Vector2 0 0) -- FIXME: trigger moved handler for html widgets

keyDownHandler '\x08' _ _ id = triggerRemoveHandler id
keyDownHandler '\x2e' _ _ id = triggerRemoveHandler id
keyDownHandler _      _ _ _  = return ()

handleSelection :: Mouse.Event' -> WidgetId -> Command Global.State ()
handleSelection evt id = case evt ^. Mouse.keyMods of
    KeyMods False False False False -> zoom Global.uiRegistry $ performSelect id
    KeyMods False False True  False -> zoom Global.uiRegistry $ toggleSelect  id
    otherwise                       -> return ()

performSelect :: WidgetId -> Command UIRegistry.State ()
performSelect id = do
    isSelected <- UICmd.get id Model.isSelected
    unless isSelected $ do
        unselectAll
        UICmd.update_ id (Model.isSelected .~ True)

toggleSelect :: WidgetId -> Command UIRegistry.State ()
toggleSelect id = UICmd.update_ id (Model.isSelected %~ not)

unselectAll :: Command UIRegistry.State ()
unselectAll = do
    widgets <- allNodes
    let widgetIds = (^. objectId) <$> widgets
    forM_ widgetIds $ (flip UICmd.update) (Model.isSelected .~ False)


dblClickHandler :: DblClickHandler Global.State
dblClickHandler _ _ id = triggerEnterNodeHandler id

selectNode evt id = do
    triggerFocusNodeHandler id
    UICmd.takeFocus id
    handleSelection evt id

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyDown      .~ keyDownHandler
                     & mousePressed .~ (\evt _ id -> selectNode evt id)
                     & dblClick     .~ dblClickHandler

allNodes :: Command UIRegistry.State [WidgetFile Model.Node]
allNodes = UIRegistry.lookupAllM

unselectNode :: WidgetId -> Command UIRegistry.State ()
unselectNode = flip UICmd.update_ (Model.isSelected .~ False)


onClicked h = addHandler (MousePressedHandler $ h) mempty

instance ResizableWidget Model.Node
instance CompositeWidget Model.Node where
    createWidget id model = do
        let label  = Style.expressionLabel $ model ^. Model.expression
        UICmd.register id label $ onClicked (\evt _ -> selectNode evt id)

        let group  = Group.create & Group.position .~ Style.controlsPosition
        controlGroups <- UICmd.register id group Style.controlsLayout

        let outPortControlGroup  = Group.create
        UICmd.register id outPortControlGroup Style.controlsLayout

        let grp    = Group.create & Group.style   .~ Style.expandedGroupStyle
                                  & Group.visible .~ (model ^. Model.isExpanded)
        expandedGroup <- UICmd.register controlGroups grp Style.expandedGroupLayout

        let label  = Style.execTimeLabel "Execution time: --"
        void $ UICmd.register expandedGroup label def

        let grp    = Group.create
        nodeGroup <- UICmd.register expandedGroup grp Style.expandedGroupLayout

        let widget = LabeledTextBox.create Style.portControlSize "Name" (model ^. Model.name)
        UICmd.register nodeGroup widget $ nameHandlers id

        withJust (model ^. Model.tpe) $ \tpe -> do
            let widget = LabeledTextBox.create Style.portControlSize "Type" (fromMaybe "" $ model ^. Model.tpe)
            UICmd.register_ nodeGroup widget $ typeHandlers id

        let grp    = Group.create
        UICmd.register expandedGroup grp Style.expandedGroupLayout

        let label  = Style.valueLabel ""
        UICmd.register controlGroups label def

        let group  = Group.create & Group.style   .~ Style.visualizationGroupStyle
                                  & Group.visible .~ (model ^. Model.isExpanded)
        UICmd.register_ controlGroups group (Layout.verticalLayoutHandler 0.0)


    updateWidget id old model = do
        controlsId <- expandedGroupId id
        exprId     <- expressionId    id
        nameTbId   <- nameTextBoxId   id
        valueId    <- valueLabelId    id
        valueVisId <- valueGroupId    id
        typeTbId   <- typeTextBoxId   id
        etId       <- execTimeLabelId id


        UICmd.update_ controlsId $ Group.visible .~ (model ^. Model.isExpanded)
        UICmd.update_ valueVisId $ Group.visible .~ (model ^. Model.isExpanded)
        UICmd.update_ exprId     $ Label.label   .~ (model ^. Model.expression)
        UICmd.update_ nameTbId   $ LabeledTextBox.value .~ (model ^. Model.name)
        UICmd.update_ valueId    $ Label.label   .~ (model ^. Model.value)
        withJust (model ^. Model.tpe) $ \tpe -> do
            withJust typeTbId $ \typeTbId -> UICmd.update_ typeTbId $ LabeledTextBox.value .~ tpe

        UICmd.update_ etId $ Label.label     .~ (fromMaybe "Execution time: --" $ (\v -> "Execution time: " <> v <> " ms") <$> (Text.pack . show) <$> model ^. Model.execTime)

        UICmd.update_ valueId  $ Label.alignment .~ (if model ^. Model.isExpanded then Label.Left else Label.Center)
        UICmd.moveX   valueId  $ if model ^. Model.isExpanded then 0.0 else -25.0


portControlsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
portControlsGroupId id = do
    expGroup <- expandedGroupId id
    (_:_:controlsId:_) <- UICmd.children expGroup
    return controlsId

outPortControlsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
outPortControlsGroupId id = do
    (_:_:groupId:_) <- UICmd.children id
    return groupId

nodeControlsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
nodeControlsGroupId id = do
    expGroup <- expandedGroupId id
    (_:controlsId:_) <- UICmd.children expGroup

    return controlsId

nameTextBoxId :: WidgetId -> Command UIRegistry.State WidgetId
nameTextBoxId id = do
    group <- nodeControlsGroupId id
    (nodeNameId:_) <- UICmd.children group
    return nodeNameId

typeTextBoxId :: WidgetId -> Command UIRegistry.State (Maybe WidgetId)
typeTextBoxId id = do
    group <- nodeControlsGroupId id
    children <- UICmd.children group
    return $ children ^? ix 1

expandedGroupId :: WidgetId -> Command UIRegistry.State WidgetId
expandedGroupId id = do
    (_:groupId:_) <- UICmd.children id
    (expandedId:_) <- UICmd.children groupId
    return expandedId

expressionId :: WidgetId -> Command UIRegistry.State WidgetId
expressionId id = do
    (exprId:_) <- UICmd.children id
    return exprId

valueGroupId :: WidgetId -> Command UIRegistry.State WidgetId
valueGroupId id = do
    (_:groupId:_) <- UICmd.children id
    (_:_:valGrpId:_) <- UICmd.children groupId
    return valGrpId

valueLabelId :: WidgetId -> Command UIRegistry.State WidgetId
valueLabelId id = do
    (_:groupId:_) <- UICmd.children id
    (_:valLabelId:_) <- UICmd.children groupId
    return valLabelId

execTimeLabelId :: WidgetId -> Command UIRegistry.State WidgetId
execTimeLabelId id = do
    expGroup <- expandedGroupId id
    (etId:_) <- UICmd.children expGroup

    return etId
