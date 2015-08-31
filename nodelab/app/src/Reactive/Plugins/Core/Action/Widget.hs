{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Widget where

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Node     (Node)
import           Object.Widget
import qualified Object.Widget  as Widget
import           Event.Event
import           Event.Mouse    (EventWidget(..))
import qualified Event.Mouse    as Mouse
import qualified Event.Keyboard as Keyboard
import           Object.UITypes
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global       as Global
import           Reactive.Plugins.Core.Action.State.UIRegistry   (WidgetMap)
import qualified Reactive.Plugins.Core.Action.State.UIRegistry   as UIRegistry
import           ThreeJS.Widget.Button ()
import           ThreeJS.Widget.Slider ()
import           Utils.CtxDynamic
import qualified JS.Camera      as Camera
import           Debug.Trace


data Action = MouseAction    { _event   :: Mouse.Event }
            | KeyboardAction { _keyEvent   :: Keyboard.Event }
            | ApplyUpdates   { _actions :: [WidgetUIUpdate] }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "WidgetAction"

toAction :: Event Node -> Maybe Action
toAction (Mouse    m) = Just $ MouseAction    m
toAction (Keyboard k) = Just $ KeyboardAction k
toAction _            = Nothing

absPosToRel :: SceneType -> Camera.Camera -> [Double] -> Vector2 Double -> Vector2 Double
absPosToRel HUD       _      mat pos = Widget.sceneToLocal pos          mat
absPosToRel Workspace camera mat pos = Widget.sceneToLocal workspacePos mat where
    workspacePos = Camera.screenToWorkspace camera (round <$> pos)

handleGeneric :: Mouse.Event -> Camera.Camera -> UIRegistry.State -> Maybe (WidgetUIUpdate, UIRegistry.State)
handleGeneric (Mouse.Event eventType absPos button _ (Just (EventWidget widgetId mat scene))) camera registry = do
    widget                <- (UIRegistry.lookup widgetId registry) :: Maybe DisplayObject
    let pos                = absPosToRel scene camera mat (fromIntegral <$> absPos)
    (uiUpdate, newWidget) <- return $ case eventType of
        Mouse.Moved       -> onMouseMove    button pos widget
        Mouse.Pressed     -> onMousePress   button pos widget
        Mouse.Released    -> onMouseRelease button pos widget
        Mouse.Clicked     -> onClick               pos widget
        Mouse.DblClicked  -> onDblClick            pos widget
    let newRegistry = UIRegistry.update newWidget registry
    return (uiUpdate, newRegistry)
handleGeneric _ _ _        = Nothing

triggerHandler :: Maybe WidgetId -> (DisplayObject -> WidgetUpdate) -> UIRegistry.State -> Maybe (WidgetUIUpdate, UIRegistry.State)
triggerHandler maybeOid handler state = do
    oid                     <- maybeOid
    oldWidget               <- UIRegistry.lookup oid state
    let (action, newWidget) = handler oldWidget
    let newState            = UIRegistry.update newWidget state
    return (action, newState)

handleDragStart (EventWidget widgetId mat scene) button keyMods absPos camera state = do
    oldWidget      <- UIRegistry.lookup widgetId state
    let pos         = absPosToRel scene camera mat absPos
    let shouldDrag  = mayDrag button pos oldWidget
    let dragState   = DragState widgetId mat scene button keyMods pos pos pos where
    let newState    = if shouldDrag then state & UIRegistry.dragState .~ Just dragState
                                    else state
    return $ (Nothing, newState)

handleDragMove keyMods absPos camera state = case (state ^. UIRegistry.dragState) of
    Just dragState -> triggerHandler widgetId (onDragMove newDragState) state' where
        widgetId           = Just $ dragState ^. Widget.widgetId
        state'             =     state & UIRegistry.dragState   .~ (Just newDragState)
        newDragState       = dragState &     Widget.previousPos .~ (dragState ^. currentPos)
                                       &     Widget.currentPos  .~ relPos
                                       &     Widget.keyMods     .~ keyMods
        relPos             = absPosToRel (dragState ^. Widget.scene) camera (dragState ^. widgetMatrix) absPos

    otherwise      -> Nothing

handleDragEnd absPos camera state = case (state ^. UIRegistry.dragState) of
    Just dragState -> do
        let widgetId = (dragState ^. Widget.widgetId)
        let relPos   = absPosToRel (dragState ^. Widget.scene) camera (dragState ^. widgetMatrix) absPos
        (actions, newState') <- triggerHandler (Just $ widgetId) (onDragEnd dragState) state
        let newState          = newState' & UIRegistry.dragState .~ Nothing
        return $ (actions, newState)
    otherwise      -> Nothing

changeFocus (EventWidget widgetId mat scene) button keyMods absPos camera state = do
    oldWidget      <- UIRegistry.lookup widgetId state
    let pos         = absPosToRel scene camera mat absPos
    let shouldFocus = mayFocus button pos oldWidget
    let newState    = if shouldFocus then state & UIRegistry.focusedWidget .~ Just widgetId
                                     else state
    return $ (Nothing, newState)

loseFocus state = Just $ (Nothing, newState) where newState = state & UIRegistry.focusedWidget .~ Nothing


handleKeyEvents :: Keyboard.Event -> UIRegistry.State -> Maybe (WidgetUIUpdate, UIRegistry.State)
handleKeyEvents (Keyboard.Event tpe ch) registry = case registry ^. UIRegistry.focusedWidget of
    Just widgetId  -> do
        widget                <- (UIRegistry.lookup widgetId registry) :: Maybe DisplayObject
        (uiUpdate, newWidget) <- return $ case tpe of
            Keyboard.Up      -> onKeyUp      ch widget
            Keyboard.Down    -> onKeyDown    ch widget
            Keyboard.Press   -> onKeyPressed ch widget
        let newRegistry = UIRegistry.update newWidget registry
        return (uiUpdate, newRegistry)
    Nothing -> Just $ (Nothing, registry)


instance ActionStateUpdater Action where
    execSt (MouseAction mouseEvent) oldState = ActionUI newAction newState where
        newAction                = ApplyUpdates uiUpdates
        newState                 = oldState &  Global.uiRegistry .~ newRegistry
        oldRegistry              = oldState ^. Global.uiRegistry
        oldWidgetOver            = oldState ^. Global.uiRegistry . UIRegistry.widgetOver
        camera                   = Global.toCamera oldState
        (uiUpdates, newRegistry) = UIRegistry.sequenceUpdates [ Just $ setWidgetOver
                                                              , handleMouseOut
                                                              , handleMouseOver
                                                              , handleFocus
                                                              , Just $ (handleGeneric mouseEvent camera)
                                                              , handleDrag
                                                              ] oldRegistry
        handleFocus = case mouseEvent of
            Mouse.Event Mouse.Pressed pos button keyMods (Just evWd) -> Just $ changeFocus evWd button keyMods (fromIntegral <$> pos) camera
            Mouse.Event Mouse.Pressed _   _      _        Nothing    -> Just $ loseFocus
            _                                                        -> Nothing
        (handleMouseOver, handleMouseOut) = case mouseEvent of
            Mouse.Event Mouse.Moved _ _ _ evWd -> case widgetOverChanged of
                                        True   -> ( Just $ triggerHandler oldWidgetOver onMouseOut
                                                  , Just $ triggerHandler newWidgetOver onMouseOver
                                                  )
                                        False  -> (Nothing, Nothing)
            _             -> (Nothing, Nothing)
        handleDrag = case mouseEvent of
            Mouse.Event Mouse.Pressed  pos button keyMods (Just evWd) -> Just $ handleDragStart evWd button keyMods (fromIntegral <$> pos) camera
            Mouse.Event Mouse.Moved    pos _      keyMods  _          -> Just $ handleDragMove              keyMods (fromIntegral <$> pos) camera
            Mouse.Event Mouse.Released pos _      _        _          -> Just $ handleDragEnd                       (fromIntegral <$> pos) camera
            _              -> Nothing
        isDragging        = isJust $ oldDragState
        oldDragState      = oldRegistry ^. UIRegistry.dragState
        widgetOverChanged = oldWidgetOver /= newWidgetOver
        newWidgetOver     = case mouseEvent of
            Mouse.Event Mouse.Moved _ _ _ evWd -> (^. Mouse.widgetId) <$> evWd
            _                                  -> oldWidgetOver
        setWidgetOver state = Just $ (Nothing, state & UIRegistry.widgetOver .~ newWidgetOver)

    execSt (KeyboardAction keyboardEvent) oldState = ActionUI newAction newState where
        newAction                = ApplyUpdates uiUpdates
        newState                 = oldState &  Global.uiRegistry .~ newRegistry
        oldRegistry              = oldState ^. Global.uiRegistry
        (uiUpdates, newRegistry) = UIRegistry.sequenceUpdates [ Just $ handleKeyEvents keyboardEvent
                                                              ] oldRegistry

instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates actions) state) = sequence_ $ catMaybes actions
