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
                                               dblClick, dragEnd, dragMove, keyDown, keyMods, keyUp, mouseOut,
                                               mouseOver, mousePressed, objectId, resizeWidget, startPos, updateWidget)

import qualified Object.Widget.Group          as Group
import qualified Object.Widget.Label          as Label
import qualified Object.Widget.LabeledTextBox as LabeledTextBox
import qualified Object.Widget.Node           as Model
import qualified Object.Widget.TextBox        as TextBox
import qualified Object.Widget.Toggle         as Toggle
import           Reactive.Commands.Command    (Command, performIO)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global
import           Reactive.State.UIRegistry    (addHandler)
import qualified Reactive.State.UIRegistry    as UIRegistry

import qualified Style.Node                   as Style
import           UI.Generic                   (defaultResize, startDrag, whenChanged)
import           UI.Handlers.Button           (MousePressedHandler (..))
import           UI.Handlers.Generic          (triggerValueChanged)
import           UI.Handlers.Generic          (ValueChangedHandler (..), triggerValueChanged)
import           UI.Handlers.LabeledTextBox   ()
import           UI.Layout                    as Layout
import           UI.Widget.Group              ()
import           UI.Widget.Label              ()
import           UI.Widget.LabeledTextBox     ()
import           UI.Widget.Node               ()
import           UI.Widget.TextBox            ()

import           Empire.API.Data.Node         (NodeId)


nameHandlers :: WidgetId -> HTMap
nameHandlers id = addHandler (ValueChangedHandler $ nameValueChangedHandler id)
                $ addHandler (UICmd.LostFocus $ inRegistry . flip UICmd.update_ (TextBox.isEditing .~ False))
                $ mempty

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

isRequiredHandlers :: WidgetId -> HTMap
isRequiredHandlers id = addHandler (ValueChangedHandler $ isRequiredHandler id)
                      $ mempty

isRequiredHandler :: WidgetId -> Bool -> WidgetId -> Command Global.State ()
isRequiredHandler parent val _ = do
    model <- inRegistry $ UICmd.update parent $ Model.isRequired .~ val
    triggerNodeRequiredHandler parent model


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

newtype ExpandNodeHandler = ExpandNodeHandler (Command Global.State ())
expandNodeHandler = TypeKey :: TypeKey ExpandNodeHandler

newtype NodeRequiredHandler = NodeRequiredHandler (NodeId -> Bool -> Command Global.State ())
nodeRequiredHandler = TypeKey :: TypeKey NodeRequiredHandler



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

triggerExpandNodeHandler :: WidgetId -> Command Global.State ()
triggerExpandNodeHandler id = do
    maybeHandler <- inRegistry $ UICmd.handler id expandNodeHandler
    forM_ maybeHandler $ \(ExpandNodeHandler handler) -> handler

triggerNodeRequiredHandler :: WidgetId -> Model.Node -> Command Global.State ()
triggerNodeRequiredHandler id model = do
    maybeHandler <- inRegistry $ UICmd.handler id nodeRequiredHandler
    forM_ maybeHandler $ \(NodeRequiredHandler handler) -> handler (model ^. Model.nodeId) (model ^. Model.isRequired)

keyDownHandler :: KeyPressedHandler Global.State
keyDownHandler '\r'   _ _ id = triggerExpandNodeHandler id
keyDownHandler '\x08' _ _ id = triggerRemoveHandler id
keyDownHandler '\x2e' _ _ id = triggerRemoveHandler id
keyDownHandler _      _ _ _  = return ()

selectNode :: Mouse.Event' -> WidgetId -> Command Global.State ()
selectNode evt id = do
    let action = handleSelection evt
    selectNode' action id

selectNode' :: (WidgetId -> Command UIRegistry.State ()) -> WidgetId -> Command Global.State ()
selectNode' action id = do
    triggerFocusNodeHandler id
    UICmd.takeFocus id
    inRegistry $ action id

handleSelection :: Mouse.Event' -> (WidgetId -> Command UIRegistry.State ())
handleSelection evt = case evt ^. Mouse.keyMods of
    KeyMods False False False False -> performSelect
    KeyMods False False True  False -> toggleSelect
    otherwise                       -> const $ return ()

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

showHidePortLabels :: Bool -> WidgetId -> Command UIRegistry.State ()
showHidePortLabels show id = do
    inLabels <- inLabelsGroupId id
    UICmd.update_ inLabels $ Group.visible .~ show
    outLabels <- outLabelsGroupId id
    UICmd.update_ outLabels $ Group.visible .~ show

onMouseOver, onMouseOut :: WidgetId -> Command Global.State ()
onMouseOver id = inRegistry $ do
    UICmd.update_ id $ Model.highlight .~ True
    showHidePortLabels True id
onMouseOut  id = inRegistry $ do
    UICmd.update_ id $ Model.highlight .~ False
    showHidePortLabels False id

widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyDown      .~ keyDownHandler
                     & mousePressed .~ (\evt _ id -> selectNode evt id)
                     & dblClick     .~ dblClickHandler
                     & mouseOver .~ const onMouseOver
                     & mouseOut  .~ const onMouseOut

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

        let inLabelsGroup  = Group.create & Group.position .~ (Vector2 (-400) (-30))
                                          & Group.visible .~ False
        UICmd.register id inLabelsGroup Style.inLabelsLayout

        let outLabelsGroup  = Group.create & Group.position .~ (Vector2 (40) (-30))
                                           & Group.visible .~ False
        UICmd.register id outLabelsGroup Style.inLabelsLayout

        let grp    = Group.create & Group.style   .~ Style.expandedGroupStyle
                                  & Group.visible .~ (model ^. Model.isExpanded)
        expandedGroup <- UICmd.register controlGroups grp Style.expandedGroupLayout

        let label  = Style.execTimeLabel "Execution time: --"
        void $ UICmd.register expandedGroup label def

        let grp    = Group.create
        nodeGroup <- UICmd.register expandedGroup grp Style.expandedGroupLayout

        let widget = LabeledTextBox.create Style.portControlSize "Name" (model ^. Model.name)
        UICmd.register nodeGroup widget $ nameHandlers id

        let widget = Toggle.create Style.portControlSize "Required" (model ^. Model.isRequired)
        UICmd.register nodeGroup widget $ isRequiredHandlers id

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
        whenChanged old model Model.isExpanded $ do
            controlsId <- expandedGroupId id
            valueVisId <- valueGroupId    id
            UICmd.update_ controlsId $ Group.visible .~ (model ^. Model.isExpanded)
            UICmd.update_ valueVisId $ Group.visible .~ (model ^. Model.isExpanded)
        whenChanged old model Model.expression $ do
            exprId     <- expressionId    id
            UICmd.update_ exprId     $ Label.label   .~ (model ^. Model.expression)
        whenChanged old model Model.name  $ do
            nameTbId   <- nameTextBoxId   id
            UICmd.update_ nameTbId   $ LabeledTextBox.value .~ (model ^. Model.name)
        whenChanged old model Model.value $ do
            valueId    <- valueLabelId    id
            UICmd.update_ valueId    $ Label.label   .~ (model ^. Model.value)
        whenChanged old model Model.tpe   $ withJust (model ^. Model.tpe) $ \tpe -> do
            typeTbId   <- typeTextBoxId   id
            withJust typeTbId $ \typeTbId -> UICmd.update_ typeTbId $ LabeledTextBox.value .~ tpe

        whenChanged old model Model.execTime $ do
            etId       <- execTimeLabelId id
            UICmd.update_ etId $ Label.label     .~ (fromMaybe "Execution time: --" $ (\v -> "Execution time: " <> v <> " ms") <$> (Text.pack . show) <$> model ^. Model.execTime)

        whenChanged old model Model.isExpanded $ do
            valueId    <- valueLabelId    id
            UICmd.update_ valueId  $ Label.alignment .~ (if model ^. Model.isExpanded then Label.Left else Label.Center)
            UICmd.moveX   valueId  $ if model ^. Model.isExpanded then 0.0 else -25.0

        whenChanged old model Model.isRequired $ do
            toggleId  <- isRequiredId  id
            UICmd.update_ toggleId $ Toggle.value .~ (model ^. Model.isRequired)


portControlsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
portControlsGroupId id = do
    expGroup <- expandedGroupId id
    (_:_:controlsId:_) <- UICmd.children expGroup
    return controlsId

outPortControlsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
outPortControlsGroupId id = do
    (_:_:groupId:_) <- UICmd.children id
    return groupId

inLabelsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
inLabelsGroupId id = do
    (_:_:_:groupId:_) <- UICmd.children id
    return groupId

outLabelsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
outLabelsGroupId id = do
    (_:_:_:_:groupId:_) <- UICmd.children id
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

isRequiredId :: WidgetId -> Command UIRegistry.State WidgetId
isRequiredId id = do
    group <- nodeControlsGroupId id
    (_:toggleId:_) <- UICmd.children group
    return toggleId

typeTextBoxId :: WidgetId -> Command UIRegistry.State (Maybe WidgetId)
typeTextBoxId id = do
    group <- nodeControlsGroupId id
    children <- UICmd.children group
    return $ children ^? ix 2

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
