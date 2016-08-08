{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module UI.Handlers.Node where

import           Utils.PreludePlus            hiding (stripPrefix)

import           Data.HMap.Lazy               (HTMap, TypeKey (..))
import           Data.Text.Lazy               (stripPrefix)
import qualified Data.Text.Lazy               as Text
import           Utils.Vector

import           Event.Keyboard               (KeyMods (..))
import qualified Event.Mouse                  as Mouse
import           Object.Widget                (CompositeWidget, DblClickHandler, KeyPressedHandler, ResizableWidget, UIHandlers, WidgetFile,
                                               WidgetId, createWidget, dblClick, keyDown, mouseOut, mouseOver, mousePressed, objectId,
                                               updateWidget, widget)

import qualified Object.Widget.Group          as Group
import qualified Object.Widget.Label          as Label
import qualified Object.Widget.LabeledTextBox as LabeledTextBox
import qualified Object.Widget.Node           as Model
import qualified Object.Widget.TextBox        as TextBox
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.Graph         as Graph
import           Reactive.State.UIRegistry    (addHandler)
import qualified Reactive.State.UIRegistry    as UIRegistry
import           Reactive.Commands.Batch           (collaborativeTouch, cancelCollaborativeTouch)

import qualified Style.Node                   as Style
import           UI.Generic                   (whenChanged)
import           UI.Handlers.Button           (MousePressedHandler (..))
import           UI.Handlers.Generic          (ValueChangedHandler (..))
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


triggerRemoveHandler :: WidgetId -> Command Global.State ()
triggerRemoveHandler id = do
    maybeHandler <- inRegistry $ UICmd.handler id removeNodeHandler
    withJust maybeHandler $ \(RemoveNodeHandler handler) -> handler

triggerFocusNodeHandler :: WidgetId -> Command Global.State ()
triggerFocusNodeHandler id = do
    maybeHandler <- inRegistry $ UICmd.handler id focusNodeHandler
    withJust maybeHandler $ \(FocusNodeHandler handler) -> handler id

triggerRenameNodeHandler :: WidgetId -> Model.Node -> Command Global.State ()
triggerRenameNodeHandler id model = do
    maybeHandler <- inRegistry $ UICmd.handler id renameNodeHandler
    withJust maybeHandler $ \(RenameNodeHandler handler) -> handler id (model ^. Model.nodeId) (model ^. Model.name)

triggerChangeInputNodeTypeHandler :: WidgetId -> Model.Node -> Command Global.State ()
triggerChangeInputNodeTypeHandler id model = do
    withJust (model ^. Model.tpe) $ \tpe -> do
        maybeHandler <- inRegistry $ UICmd.handler id changeInputNodeTypeHandler
        withJust maybeHandler $ \(ChangeInputNodeTypeHandler handler) -> handler id (model ^. Model.nodeId) tpe

triggerEnterNodeHandler :: WidgetId -> Command Global.State ()
triggerEnterNodeHandler id = do
    maybeHandler <- inRegistry $ UICmd.handler id enterNodeHandler
    withJust maybeHandler $ \(EnterNodeHandler handler) -> handler

triggerExpandNodeHandler :: WidgetId -> Command Global.State ()
triggerExpandNodeHandler id = do
    maybeHandler <- inRegistry $ UICmd.handler id expandNodeHandler
    withJust maybeHandler $ \(ExpandNodeHandler handler) -> handler

keyDownHandler :: KeyPressedHandler Global.State
keyDownHandler '\r'   _ _ id = triggerExpandNodeHandler id
keyDownHandler '\x08' _ _ id = triggerRemoveHandler id
keyDownHandler '\x2e' _ _ id = triggerRemoveHandler id
keyDownHandler _      _ _ _  = return ()

selectNode :: Mouse.Event' -> WidgetId -> Command Global.State ()
selectNode evt id = do
    let action = handleSelection evt
    selectNode' action id

selectNode' :: (WidgetId -> Command Global.State ()) -> WidgetId -> Command Global.State ()
selectNode' action id = do
    triggerFocusNodeHandler id
    UICmd.takeFocus id
    action id

handleSelection :: Mouse.Event' -> (WidgetId -> Command Global.State ())
handleSelection evt = case evt ^. Mouse.keyMods of
    KeyMods False False False False -> performSelect
    KeyMods False False True  False -> toggleSelect
    otherwise                       -> const $ return ()

performSelect :: WidgetId -> Command Global.State ()
performSelect id = do
    isSelected <- inRegistry $ UICmd.get id Model.isSelected
    nodeId     <- inRegistry $ UICmd.get id Model.nodeId
    unless isSelected $ do
        unselectAll
        inRegistry $ UICmd.update_ id (Model.isSelected .~ True)
        collaborativeTouch [nodeId]

toggleSelect :: WidgetId -> Command Global.State ()
toggleSelect id = do
    newNode <- inRegistry $ UICmd.update id (Model.isSelected %~ not)
    let nodeId = newNode ^. Model.nodeId
    if newNode ^. Model.isSelected then
      collaborativeTouch [nodeId]
    else
      cancelCollaborativeTouch [nodeId]


unselectAll :: Command Global.State ()
unselectAll = do
    widgets <- allNodes
    nodesToCancelTouch <- inRegistry $ flip mapM widgets $ \wf -> do
        let widgetId = wf ^. objectId
        if (wf ^. widget . Model.isSelected) then do
                UICmd.update_ widgetId $ Model.isSelected .~ False
                return $ Just $ wf ^. widget . Model.nodeId
        else return Nothing

    cancelCollaborativeTouch $ catMaybes nodesToCancelTouch

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

allNodes :: Command Global.State [WidgetFile Model.Node]
allNodes = do
    widgetIds <- use $ Global.graph . Graph.nodeWidgets
    mayWidgets <- mapM (\id -> inRegistry $ UIRegistry.lookupTypedM id) widgetIds
    return $ catMaybes mayWidgets

unselectNode :: WidgetId -> Command UIRegistry.State ()
unselectNode = flip UICmd.update_ (Model.isSelected .~ False)


onClicked h = addHandler (MousePressedHandler $ h) mempty



instance ResizableWidget Model.Node
instance CompositeWidget Model.Node where
    createWidget id model = do
        let grp    = Group.create & Group.size .~ (Vector2 1 1)
        portGroup <- UICmd.register id grp def

        let label = Style.expressionLabel $ mockupExpression $ model ^. Model.expression
        expressionLabelId <- UICmd.register id label $ onClicked (\evt _ -> selectNode evt id)

        let group  = Group.create & Group.position .~ Style.controlsPosition
        controlGroups <- UICmd.register id group Style.controlsLayout

        let inLabelsGroup  = Group.create & Group.position .~ (Vector2 (-400) (-30))
                                          & Group.visible .~ False
        inLabelsGroupId <- UICmd.register id inLabelsGroup Style.inLabelsLayout

        let outLabelsGroup  = Group.create & Group.position .~ (Vector2 (40) (-30))
                                           & Group.visible .~ False
        outLabelsGroupId <- UICmd.register id outLabelsGroup Style.inLabelsLayout

        let grp    = Group.create & Group.style   .~ Style.expandedGroupStyle
                                  & Group.visible .~ (model ^. Model.isExpanded)
        expandedGroup <- UICmd.register controlGroups grp Style.expandedGroupLayout

        let label  = Style.execTimeLabel "Execution time: --"
        execTimeLabelId <- UICmd.register expandedGroup label def

        nodeGroupId <- UICmd.register expandedGroup (Group.create) Style.expandedGroupLayout

        let widget = LabeledTextBox.create Style.portControlSize "Name" $ model ^. Model.name
        nameTextBoxId <- UICmd.register nodeGroupId widget $ nameHandlers id

        withJust (model ^. Model.tpe) $ \tpe -> do
            let widget = LabeledTextBox.create Style.portControlSize "Type" (fromMaybe "" $ model ^. Model.tpe)
            nodeTpeId <- UICmd.register nodeGroupId widget $ typeHandlers id
            void $ UIRegistry.updateWidgetM id $ Model.elements . Model.nodeType     ?~ nodeTpeId

        let grp    = Group.create
        portControlsGroupId <- UICmd.register expandedGroup grp Style.expandedGroupLayout

        let label  = Style.valueLabel ""
        valueLabelId <- UICmd.register controlGroups label def

        let group  = Group.create & Group.style   .~ Style.visualizationGroupStyle
                                  & Group.visible .~ (model ^. Model.isExpanded)
                                  & Group.size    . y .~ 0
        visualizationGroupId <- UICmd.register controlGroups group (Layout.verticalLayoutHandler 0.0)

        void $ UIRegistry.updateWidgetM id $ Model.elements %~ ( (Model.expressionLabel    .~ expressionLabelId    )
                                                               . (Model.expandedGroup      .~ expandedGroup        )
                                                               . (Model.portGroup          .~ portGroup            )
                                                               . (Model.portControls       .~ portControlsGroupId  )
                                                               . (Model.inLabelsGroup      .~ inLabelsGroupId      )
                                                               . (Model.outLabelsGroup     .~ outLabelsGroupId     )
                                                               . (Model.nameTextBox        .~ nameTextBoxId        )
                                                               . (Model.valueLabel         .~ valueLabelId         )
                                                               . (Model.visualizationGroup .~ visualizationGroupId )
                                                               . (Model.execTimeLabel      .~ execTimeLabelId      )
                                                               )

    updateWidget id old model = do
        whenChanged old model Model.isExpanded $ do
            let controlsId = model ^. Model.elements . Model.expandedGroup
                valueVisId = model ^. Model.elements . Model.visualizationGroup
            UICmd.update_ controlsId $ Group.visible .~ (model ^. Model.isExpanded)
            UICmd.update_ valueVisId $ Group.visible .~ (model ^. Model.isExpanded)

        whenChanged old model Model.expression $ do
            let exprId = model ^. Model.elements . Model.expressionLabel

            UICmd.update_ exprId     $ Label.label   .~ (mockupExpression $ model ^. Model.expression)

        whenChanged old model Model.name  $ do
            let nameTbId = model ^. Model.elements . Model.nameTextBox
            UICmd.update_ nameTbId   $ LabeledTextBox.value .~ (model ^. Model.name)

        whenChanged old model Model.value $ do
            let valueId = model ^. Model.elements . Model.valueLabel
            UICmd.update_ valueId    $ Label.label   .~ (model ^. Model.value)

        whenChanged old model Model.tpe   $ withJust (model ^. Model.tpe) $ \tpe -> do
            let typeTbId = model ^. Model.elements . Model.nodeType
            withJust typeTbId $ \typeTbId -> UICmd.update_ typeTbId $ LabeledTextBox.value .~ tpe

        whenChanged old model Model.execTime $ do
            let etId = model ^. Model.elements . Model.execTimeLabel
            UICmd.update_ etId $ Label.label     .~ (fromMaybe "Execution time: --" $ (\v -> "Execution time: " <> v <> " ms") <$> (Text.pack . show) <$> model ^. Model.execTime)

        whenChanged old model Model.isExpanded $ do
            let valueId = model ^. Model.elements . Model.valueLabel
            UICmd.update_ valueId  $ Label.alignment .~ (if model ^. Model.isExpanded then Label.Left else Label.Center)
            UICmd.moveX   valueId  $ if model ^. Model.isExpanded then 0.0 else -25.0


portControlsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
portControlsGroupId id = UICmd.get id $ Model.elements . Model.portControls

inLabelsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
inLabelsGroupId id = UICmd.get id $ Model.elements . Model.inLabelsGroup

outLabelsGroupId :: WidgetId -> Command UIRegistry.State WidgetId
outLabelsGroupId id = UICmd.get id $ Model.elements . Model.outLabelsGroup

expressionId :: WidgetId -> Command UIRegistry.State WidgetId
expressionId id = UICmd.get id $ Model.elements . Model.expressionLabel

valueGroupId :: WidgetId -> Command UIRegistry.State WidgetId
valueGroupId id = UICmd.get id $ Model.elements . Model.visualizationGroup

mockupExpression :: Text -> Text
mockupExpression (stripPrefix "inside "                  -> Just name) = "inside"
mockupExpression (stripPrefix "outside "                 -> Just name) = "outside"
mockupExpression (stripPrefix "temperatureThreshold "    -> Just name) = "temperatureThreshold"
mockupExpression name = name
