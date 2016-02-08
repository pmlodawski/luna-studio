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
import           UI.Handlers.Generic          (ValueChangedHandler (..), triggerValueChanged)
import           UI.Layout                    as Layout
import           UI.Widget.Group              ()
import           UI.Widget.Label              ()
import           UI.Widget.Node               ()
import           UI.Widget.TextBox            ()
import           UI.Widget.LabeledTextBox     ()
import           UI.Handlers.LabeledTextBox   ()

import           Empire.API.Data.Node         (NodeId)


textHandlers :: WidgetId -> HTMap
textHandlers id = addHandler (ValueChangedHandler $ textValueChangedHandler id)
                $ addHandler (UICmd.LostFocus $ inRegistry . flip UICmd.update_ (TextBox.isEditing .~ False))
                $ mempty where

textValueChangedHandler :: WidgetId -> Text -> WidgetId -> Command Global.State ()
textValueChangedHandler parent val tbId = do
    model <- inRegistry $ UICmd.update parent $ Model.name .~ val
    triggerRenameNodeHandler parent model

newtype RemoveNodeHandler = RemoveNodeHandler (Command Global.State ())
removeNodeHandler = TypeKey :: TypeKey RemoveNodeHandler

newtype FocusNodeHandler = FocusNodeHandler (WidgetId -> Command Global.State ())
focusNodeHandler = TypeKey :: TypeKey FocusNodeHandler

newtype RenameNodeHandler = RenameNodeHandler (WidgetId -> NodeId -> Text -> Command Global.State ())
renameNodeHandler = TypeKey :: TypeKey RenameNodeHandler

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

triggerEnterNodeHandler :: WidgetId -> Command Global.State ()
triggerEnterNodeHandler id = do
    maybeHandler <- inRegistry $ UICmd.handler id enterNodeHandler
    forM_ maybeHandler $ \(EnterNodeHandler handler) -> handler

keyDownHandler :: KeyPressedHandler Global.State
keyDownHandler '\r'   _ _ id = inRegistry $ UICmd.update_ id (Model.isExpanded %~ not)
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

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyDown      .~ keyDownHandler
                     & mousePressed .~ (\evt _ id -> do
                         triggerFocusNodeHandler id
                         UICmd.takeFocus id
                         handleSelection evt id)
                     & dblClick     .~ dblClickHandler

allNodes :: Command UIRegistry.State [WidgetFile Model.Node]
allNodes = UIRegistry.lookupAllM

unselectNode :: WidgetId -> Command UIRegistry.State ()
unselectNode = flip UICmd.update_ (Model.isSelected .~ False)

instance ResizableWidget Model.Node
instance CompositeWidget Model.Node where
    createWidget id model = do
        let offset = Vector2 (-50.0) (-50.0)
            label  = Label.Label offset (Vector2 100.0 20.0) Label.Center $ model ^. Model.expression
        void $ UICmd.register id label def

        let textBox = Label.Label (Vector2 (-40.0) (-10.0)) (Vector2 80.0 20.0) Label.Center (model ^. Model.name)
        void $ UICmd.register id textBox def

        let offset = Vector2 (-30.0) 40.0
            group  = Group.create & Group.position .~ offset
                                  & Group.visible  .~ (model ^. Model.isExpanded)
        controlGroups <- UICmd.register id group (Layout.verticalLayoutHandler def 5.0)

        let grp    = Group.Group def def True $ Just (0.2, 0.2, 0.2)
        expandedGroup <- UICmd.register controlGroups grp (Layout.verticalLayoutHandler (Vector2 5.0 5.0) 5.0)

        let grp    = Group.create
        nodeGroup <- UICmd.register expandedGroup grp (Layout.verticalLayoutHandler def 5.0)

        let widget = LabeledTextBox.create (Vector2 200 20) "Name" (model ^. Model.name)
        UICmd.register nodeGroup widget $ textHandlers id

        let grp    = Group.create
        UICmd.register expandedGroup grp (Layout.verticalLayoutHandler def 5.0)

        let label  = Label.Label (Vector2 (-50.0) 0.0) (Vector2 100.0 20.0) Label.Left ""
        UICmd.register controlGroups label def

        let group  = Group.create & Group.background ?~ (0.2, 0.2, 0.2)
                                  & Group.visible    .~ (model ^. Model.isExpanded)
        UICmd.register_ controlGroups group (Layout.verticalLayoutHandler def 0.0)

    updateWidget id old model = do
        controlsId <- expandedGroupId id
        exprId     <- expressionId    id
        nameId     <- nameLabelId     id
        nameTbId   <- nameTextBoxId   id
        valueId    <- valueLabelId    id
        valueVisId <- valueGroupId    id

        UICmd.update_ controlsId $ Group.visible .~ (model ^. Model.isExpanded)
        UICmd.update_ valueVisId $ Group.visible .~ (model ^. Model.isExpanded)
        UICmd.update_ exprId     $ Label.label   .~ (model ^. Model.expression)
        UICmd.update_ nameId     $ Label.label   .~ (model ^. Model.name)
        UICmd.update_ nameTbId   $ LabeledTextBox.value .~ (model ^. Model.name)
        UICmd.update_ valueId    $ Label.label   .~ (model ^. Model.value)


-- Node widget structure:
-- Node:
--   1. expression label
--   2. name label
--   3. group
--      1. expanded group
--        1. node controls [name, etc.]
--        2. port controls [name, etc.]
--      2. Expression value
--      3. Visualization group
--   4.. ports (should put them into separate group widget)

portControlsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
portControlsGroupId id = do
    expGroup <- expandedGroupId id
    (_:controlsId:_) <- UICmd.children expGroup
    return controlsId

nodeControlsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
nodeControlsGroupId id = do
    expGroup <- expandedGroupId id
    (controlsId:_) <- UICmd.children expGroup

    return controlsId

nameTextBoxId :: WidgetId -> Command UIRegistry.State WidgetId
nameTextBoxId id = do
    group <- nodeControlsGroupId id
    (nodeNameId:_) <- UICmd.children group
    return nodeNameId

expandedGroupId :: WidgetId -> Command UIRegistry.State WidgetId
expandedGroupId id = do
    (_:_:groupId:_) <- UICmd.children id
    (expandedId:_) <- UICmd.children groupId
    return expandedId

expressionId :: WidgetId -> Command UIRegistry.State WidgetId
expressionId id = do
    (exprId:_) <- UICmd.children id
    return exprId

nameLabelId :: WidgetId -> Command UIRegistry.State WidgetId
nameLabelId id = do
    (_:nameId:_) <- UICmd.children id
    return nameId

valueGroupId :: WidgetId -> Command UIRegistry.State WidgetId
valueGroupId id = do
    (_:_:groupId:_) <- UICmd.children id
    (_:_:valGrpId:_) <- UICmd.children groupId
    return valGrpId

valueLabelId :: WidgetId -> Command UIRegistry.State WidgetId
valueLabelId id = do
    (_:_:groupId:_) <- UICmd.children id
    (_:valLabelId:_) <- UICmd.children groupId
    return valLabelId
