{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Widget where

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Node     (Node)
import           Object.Widget
import qualified Object.Widget  as Widget
import           Event.Event
import           Event.Mouse    (EventWidget(..), WidgetId)
import qualified Event.Mouse    as Mouse
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global       as Global
import           Reactive.Plugins.Core.Action.State.UIRegistry   (WidgetMap)
import qualified Reactive.Plugins.Core.Action.State.UIRegistry   as UIRegistry
import           ThreeJS.Types (SceneType(..))
import           ThreeJS.Button ()
import           ThreeJS.Slider ()
import           Utils.CtxDynamic
import qualified JS.Camera      as Camera


data Action = MouseAction   { _event   :: Mouse.Event }
            | ApplyUpdates  { _actions :: [WidgetUIUpdate] }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "WidgetAction"

toAction :: Event Node -> Maybe Action
toAction (Mouse m) = Just $ MouseAction m
toAction _         = Nothing

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
    return (uiUpdate, registry)
handleGeneric _ _ _        = Nothing

triggerHandler :: Maybe WidgetId -> (DisplayObject -> WidgetUpdate) -> UIRegistry.State -> Maybe (WidgetUIUpdate, UIRegistry.State)
triggerHandler maybeOid handler state = do
    oid                     <- maybeOid
    oldWidget               <- UIRegistry.lookup oid state
    let (action, newWidget) = handler oldWidget
    let newState            = UIRegistry.update newWidget state
    return (action, newState)

handleDragStart (EventWidget widgetId mat scene) button absPos camera state = do
    oldWidget      <- UIRegistry.lookup widgetId state
    let pos         = absPosToRel scene camera mat absPos
    let shouldDrag  = mayDrag button pos oldWidget
    let dragState   = DragState widgetId mat scene button pos pos pos where
    let newState    = if shouldDrag then state & UIRegistry.dragState .~ Just dragState
                                    else state
    return $ (Nothing, newState)

handleDragMove absPos camera state = case (state ^. UIRegistry.dragState) of
    Just dragState -> triggerHandler widgetId (onDragMove newDragState) state' where
        widgetId           = Just $ dragState ^. Widget.widgetId
        state'             = state     & UIRegistry.dragState .~ (Just newDragState)
        newDragState       = dragState & Widget.previousPos .~ (dragState ^. currentPos)
                                       & Widget.currentPos  .~ relPos
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

instance ActionStateUpdater Action where
    execSt (MouseAction mouseEvent) oldState = case newAction of
            Just action -> ActionUI newAction newState
            Nothing     -> ActionUI  NoAction newState
        where
        newAction                = Just $ ApplyUpdates uiUpdates
        newState                 = oldState &  Global.uiRegistry .~ newRegistry
        oldRegistry              = oldState ^. Global.uiRegistry
        oldWidgetOver            = oldState ^. Global.uiRegistry . UIRegistry.widgetOver
        camera                   = Global.toCamera oldState
        (uiUpdates, newRegistry) = UIRegistry.sequenceUpdates [ Just $ setWidgetOver
                                                              , handleMouseOut
                                                              , handleMouseOver
                                                              , Just $ (handleGeneric mouseEvent camera)
                                                              , handleDrag
                                                              ] oldRegistry
        (handleMouseOver, handleMouseOut) = case mouseEvent of
            Mouse.Event Mouse.Moved _ _ _ evWd -> case widgetOverChanged of
                                        True   -> (Just $ triggerHandler oldWidgetOver onMouseOut
                                                  ,Just $ triggerHandler newWidgetOver onMouseOver
                                                  )
                                        False  -> (Nothing, Nothing)
            _             -> (Nothing, Nothing)
        handleDrag = case mouseEvent of
            Mouse.Event Mouse.Pressed  pos button _ (Just evWd) -> Just $ handleDragStart evWd button (fromIntegral <$> pos) camera
            Mouse.Event Mouse.Moved    pos _ _ _ -> Just $ handleDragMove (fromIntegral <$> pos) camera
            Mouse.Event Mouse.Released pos _ _ _ -> Just $ handleDragEnd  (fromIntegral <$> pos) camera
            _              -> Nothing
        isDragging        = isJust $ oldDragState
        oldDragState      = oldRegistry ^. UIRegistry.dragState
        widgetOverChanged = oldWidgetOver /= newWidgetOver
        newWidgetOver     = case mouseEvent of
            Mouse.Event Mouse.Moved _ _ _ evWd -> (^. Mouse.widgetId) <$> evWd
            _                                  -> oldWidgetOver
        setWidgetOver state = Just $ (Nothing, state & UIRegistry.widgetOver .~ newWidgetOver)

instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates actions) state) = sequence_ $ catMaybes actions
