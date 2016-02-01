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
                                               UIHandlers, WidgetFile, WidgetId, createWidget, currentPos, dblClick,
                                               dragEnd, dragMove, keyDown, keyMods, keyUp, mousePressed, objectId,
                                               resizeWidget, startPos, updateWidget)

import qualified Object.Widget.Group          as Group
import qualified Object.Widget.Label          as Label
import qualified Object.Widget.Node           as Model
import qualified Object.Widget.TextBox        as TextBox
import           Reactive.Commands.Command    (Command, performIO)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global        (inRegistry)
import qualified Reactive.State.Global        as Global
import           Reactive.State.UIRegistry    (addHandler)
import qualified Reactive.State.UIRegistry    as UIRegistry

import           UI.Generic                   (defaultResize, startDrag, takeFocus)
import           UI.Handlers.Generic          (triggerValueChanged)
import           UI.Handlers.Generic          (ValueChangedHandler (..), triggerValueChanged)
import           UI.Layout                    as Layout
import           UI.Widget.Group              ()
import           UI.Widget.Label              ()
import           UI.Widget.Node               ()
import           UI.Widget.TextBox            ()

import           Empire.API.Data.Node         (NodeId)


textHandlers :: WidgetId -> HTMap
textHandlers id = addHandler (ValueChangedHandler $ textValueChangedHandler id)
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
dblClickHandler _ _ id = do
    (controlsId:exprId:nameId:valueId:_) <- inRegistry $ UICmd.children id
    takeFocus undefined nameId
    inRegistry $ UICmd.update_ nameId $ TextBox.isEditing .~ True


widgetHandlers :: UIHandlers Global.State
widgetHandlers = def & keyDown      .~ keyDownHandler
                     & mousePressed .~ (\evt _ id -> do
                         triggerFocusNodeHandler id
                         takeFocus evt id
                         handleSelection evt id)
                     & dblClick     .~ dblClickHandler

allNodes :: Command UIRegistry.State [WidgetFile Model.Node]
allNodes = UIRegistry.lookupAllM

unselectNode :: WidgetId -> Command UIRegistry.State ()
unselectNode = flip UICmd.update_ (Model.isSelected .~ False)

instance ResizableWidget Model.Node
instance CompositeWidget Model.Node where
    createWidget id model = do
        let offset = Vector2 (-30.0) 65.0
            grp    = Group.Group offset def True $ Just (0.2, 0.2, 0.2)
        UICmd.register id grp (Layout.verticalLayoutHandler (Vector2 5.0 5.0) 5.0)

        let offset = Vector2 (-50.0) (-50.0)
            label  = Label.Label offset (Vector2 100.0 20.0) Label.Center $ model ^. Model.expression
        void $ UICmd.register id label def

        let textBox = TextBox.create (Vector2 80.0 20.0) (model ^. Model.name) TextBox.Center
                    & TextBox.position .~ Vector2 (-40.0) (-10.0)
        void $ UICmd.register id textBox $ textHandlers id

        let offset = Vector2 (-50.0) 35.0
            label  = Label.Label offset (Vector2 100.0 20.0) Label.Center "(pending)"
        void $ UICmd.register id label def

        let offset = Vector2 (-50.0) 55.0
            group  = Group.create & Group.position .~ offset
        void $ UICmd.register id group (Layout.verticalLayoutHandler def 0.0)

    updateWidget id old model = do
        (controlsId:exprId:nameId:valueId:_) <- UICmd.children id
        UICmd.update_ controlsId $ Group.visible .~ (model ^. Model.isExpanded)
        UICmd.update_ exprId     $ Label.label   .~ (model ^. Model.expression)
        UICmd.update_ nameId     $ TextBox.value .~ (model ^. Model.name)
        UICmd.update_ valueId    $ Label.label   .~ (model ^. Model.value)

