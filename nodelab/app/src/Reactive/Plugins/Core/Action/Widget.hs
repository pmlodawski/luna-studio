{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Widget where

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Widget
import qualified Object.Widget  as Widget
import           Event.Event
import           Event.Mouse    (EventWidget(..), MouseButton)
import qualified Event.Mouse    as Mouse
import qualified Event.Keyboard as Keyboard
import           Object.UITypes
import qualified Reactive.State.Global       as Global
import           Reactive.State.UIRegistry   (WidgetMap)
import qualified Reactive.State.UIRegistry   as UIRegistry
import qualified Reactive.State.Camera       as Camera
import           Reactive.Commands.Command   (Command, execCommand, performIO)
import           Object.Widget.Port   ()
import           Control.Monad.State

data Action = MouseAction    { _event    :: Mouse.Event    }
            | KeyboardAction { _keyEvent :: Keyboard.Event }
makeLenses ''Action

type UIRegistryState   = UIRegistry.State Global.State
type UIRegistryUpdate  = (WidgetUIUpdate, UIRegistryState)
type UIRegistryHandler = (UIRegistryState -> Maybe UIRegistryUpdate)

toAction :: Event -> Maybe (Command Global.State ())
toAction (Mouse    m) = Just . toCommand $ MouseAction    m
toAction (Keyboard k) = Just . toCommand $ KeyboardAction k
toAction _            = Nothing

toCommand :: Action -> Command Global.State ()
toCommand act = do
    (io, newState) <- gets $ execSt act
    performIO io
    put newState

absPosToRel :: SceneType -> Camera.Camera -> [Double] -> Vector2 Double -> Vector2 Double
absPosToRel HUD       _      mat pos = Widget.sceneToLocal pos          mat
absPosToRel Workspace camera mat pos = Widget.sceneToLocal workspacePos mat where
    workspacePos = Camera.screenToWorkspace camera (round <$> pos)

handleGeneric :: Mouse.Event -> Camera.Camera -> UIRegistryState -> Maybe UIRegistryUpdate
handleGeneric (Mouse.Event (Mouse.Wheel _) _ _ _ _) _ _ = Nothing
handleGeneric (Mouse.Event eventType absPos button keymods (Just (EventWidget widgetId mat scene))) camera registry = do
    file                  <- (UIRegistry.lookup widgetId registry) :: Maybe (WidgetFile Global.State DisplayObject)
    let dynamicWidget      = file ^. widget
    let pos                = absPosToRel scene camera mat (fromIntegral <$> absPos)
    (uiUpdate, newWidget) <- return $ case eventType of
        Mouse.Moved       -> onMouseMove            button  pos file dynamicWidget
        Mouse.Pressed     -> onMousePress   keymods button  pos file dynamicWidget
        Mouse.Released    -> onMouseRelease         button  pos file dynamicWidget
        Mouse.Clicked     -> onClick                        pos file dynamicWidget
        Mouse.DblClicked  -> onDblClick                     pos file dynamicWidget
    let newRegistry = UIRegistry.update widgetId newWidget registry
    return (uiUpdate, newRegistry)
handleGeneric _ _ _        = Nothing

triggerHandler :: Maybe WidgetId
               -> (WidgetFile Global.State DisplayObject
               -> DisplayObject -> WidgetUpdate)
               -> UIRegistryState
               -> Maybe UIRegistryUpdate
triggerHandler maybeOid handler state = do
    oid                     <- maybeOid
    file                    <- UIRegistry.lookup oid state
    let (action, newWidget)  = handler file (file ^. widget)
    let newState             = UIRegistry.update oid newWidget state
    return (action, newState)

handleDragStart :: EventWidget
                -> MouseButton
                -> Keyboard.KeyMods
                -> Vector2 Double
                -> Camera.Camera
                -> UIRegistryState
                -> Maybe UIRegistryUpdate
handleDragStart (EventWidget widgetId mat scene) button keyMods absPos camera state = do
    file           <- UIRegistry.lookup widgetId state
    let pos         = absPosToRel scene camera mat absPos
    let shouldDrag  = mayDrag button pos file (file ^. widget)
    let dragState   = DragState widgetId mat scene button keyMods pos pos pos where
    let newState    = if shouldDrag then state & UIRegistry.dragState .~ Just dragState
                                    else state
    return $ (noUIUpdate, newState)

handleDragMove :: Keyboard.KeyMods -> Vector2 Double -> Camera.Camera -> UIRegistryState -> Maybe UIRegistryUpdate
handleDragMove keyMods absPos camera state = case (state ^. UIRegistry.dragState) of
    Just dragState -> triggerHandler widgetId (onDragMove newDragState) state' where
        widgetId           = Just $ dragState ^. Widget.widgetId
        state'             =     state & UIRegistry.dragState   .~ (Just newDragState)
        newDragState       = dragState &     Widget.previousPos .~ (dragState ^. currentPos)
                                       &     Widget.currentPos  .~ relPos
                                       &     Widget.keyMods     .~ keyMods
        relPos             = absPosToRel (dragState ^. Widget.scene) camera (dragState ^. widgetMatrix) absPos

    otherwise      -> Nothing

handleDragEnd :: Vector2 Double -> Camera.Camera -> UIRegistryState -> Maybe UIRegistryUpdate
handleDragEnd absPos camera state = case (state ^. UIRegistry.dragState) of
    Just dragState -> do
        let widgetId = (dragState ^. Widget.widgetId)
        let relPos   = absPosToRel (dragState ^. Widget.scene) camera (dragState ^. widgetMatrix) absPos
        (actions, newState') <- triggerHandler (Just $ widgetId) (onDragEnd dragState) state
        let newState          = newState' & UIRegistry.dragState .~ Nothing
        return $ (actions, newState)
    otherwise      -> Nothing

changeFocus :: EventWidget
            -> MouseButton
            -> Keyboard.KeyMods
            -> Vector2 Double
            -> Camera.Camera
            -> UIRegistryState
            -> Maybe UIRegistryUpdate
changeFocus (EventWidget widgetId mat scene) button keyMods absPos camera state = do
    file           <- UIRegistry.lookup widgetId state
    let pos         = absPosToRel scene camera mat absPos
    let shouldFocus = mayFocus button pos file (file ^. widget)
    let newState    = if shouldFocus then state & UIRegistry.focusedWidget .~ Just widgetId
                                     else state
    return $ (noUIUpdate, newState)

loseFocus :: UIRegistryState -> Maybe UIRegistryUpdate
loseFocus state = Just $ (noUIUpdate, newState) where newState = state & UIRegistry.focusedWidget .~ Nothing

handleKeyEvents :: Keyboard.Event -> UIRegistryState -> Maybe UIRegistryUpdate
handleKeyEvents (Keyboard.Event eventType ch mods) registry = case registry ^. UIRegistry.focusedWidget of
    Just widgetId  -> do
        file                  <- (UIRegistry.lookup widgetId registry) :: Maybe (WidgetFile Global.State DisplayObject)
        (uiUpdate, newWidget) <- return $ case eventType of
            Keyboard.Up       -> onKeyUp      ch mods file (file ^. widget)
            Keyboard.Down     -> onKeyDown    ch mods file (file ^. widget)
            Keyboard.Press    -> onKeyPressed ch mods file (file ^. widget)
        let newRegistry = UIRegistry.update widgetId newWidget registry
        return (uiUpdate, newRegistry)
    Nothing -> Just $ (noUIUpdate, registry)

applyHandlers :: [Command Global.State ()] -> Global.State -> (IO (), Global.State)
applyHandlers handlers st = foldr apply (noUIUpdate, st) handlers where
    apply h (acts, st) = (acts >> act, st') where (act, st') = execCommand h st

customMouseHandlers :: Mouse.Event -> Camera.Camera -> UIRegistryState -> [Command Global.State ()]
customMouseHandlers (Mouse.Event eventType absPos button keymods (Just (EventWidget widgetId mat scene))) camera registry =
    case UIRegistry.lookupHandlers widgetId registry of
        Just handlers -> case eventType of
                Mouse.Moved       -> fmap (\a -> a         button  pos widgetId) (handlers ^. mouseMove    )
                Mouse.Pressed     -> fmap (\a -> a keymods button  pos widgetId) (handlers ^. mousePressed )
                Mouse.Released    -> fmap (\a -> a         button  pos widgetId) (handlers ^. mouseReleased)
                Mouse.Clicked     -> fmap (\a -> a keymods         pos widgetId) (handlers ^. click        )
                Mouse.DblClicked  -> fmap (\a -> a                 pos widgetId) (handlers ^. dblClick     )
                _                 -> []
            where pos              = absPosToRel scene camera mat (fromIntegral <$> absPos)
        Nothing -> []
customMouseHandlers _ _ _  = []

customKeyboardHandlers :: Keyboard.Event -> UIRegistryState -> [Command Global.State ()]
customKeyboardHandlers (Keyboard.Event eventType ch mods) registry = case registry ^. UIRegistry.focusedWidget of
    Just widgetId -> case UIRegistry.lookupHandlers widgetId registry of
        Just handlers -> case eventType of
            Keyboard.Up       -> fmap (\a -> a ch mods widgetId) (handlers ^. keyUp     )
            Keyboard.Down     -> fmap (\a -> a ch mods widgetId) (handlers ^. keyDown   )
            Keyboard.Press    -> fmap (\a -> a ch mods widgetId) (handlers ^. keyPressed)
        Nothing -> []
    Nothing -> []

customDragMoveHandlers :: Mouse.Event -> UIRegistryState -> [Command Global.State ()]
customDragMoveHandlers (Mouse.Event Mouse.Moved _ _ _ _) registry = case (registry ^. UIRegistry.dragState) of
    Just dragState -> let widgetId = (dragState ^. Widget.widgetId) in
        case UIRegistry.lookupHandlers widgetId registry of
            Just handlers -> fmap (\a -> a widgetId) (handlers ^. dragMove)
            Nothing       -> []
    Nothing -> []
customDragMoveHandlers _ _ = []

customDragEndHandlers :: Mouse.Event -> UIRegistryState -> [Command Global.State ()]
customDragEndHandlers (Mouse.Event Mouse.Released _ _ _ _) registry = case (registry ^. UIRegistry.dragState) of
    Just dragState -> let widgetId = (dragState ^. Widget.widgetId) in
        case UIRegistry.lookupHandlers widgetId registry of
            Just handlers -> fmap (\a -> a widgetId) (handlers ^. dragEnd)
            Nothing       -> []
    Nothing -> []
customDragEndHandlers _ _ = []

execSt :: Action -> Global.State -> (IO (), Global.State)
execSt (MouseAction mouseEvent) oldState = (newAction, newState') where
    newAction                    = uiUpdates >> customUIUpdates
    newState                     = oldState &  Global.uiRegistry .~ newRegistry
    (customUIUpdates, newState') = applyHandlers customHandlers newState
    oldRegistry                  = oldState ^. Global.uiRegistry
    oldWidgetOver                = oldState ^. Global.uiRegistry . UIRegistry.widgetOver
    camera                       = oldState ^. Global.camera . Camera.camera
    customHandlers               = (customMouseHandlers    mouseEvent camera newRegistry)
                                ++ (customDragMoveHandlers mouseEvent        newRegistry)
                                ++ (customDragEndHandlers  mouseEvent        oldRegistry) -- old is required, as i new dragState will be Nothing
    (uiUpdates, newRegistry)     = UIRegistry.sequenceUpdates [ Just $ setWidgetOver
                                                              , handleMouseOut
                                                              , handleMouseOver
                                                              , handleFocus
                                                              , Just $ (handleGeneric mouseEvent camera)
                                                              , handleDrag
                                                              ] oldRegistry where
        handleMouseOver, handleMouseOut, handleFocus, handleDrag :: Maybe UIRegistryHandler
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

        isDragging :: Bool
        isDragging        = isJust $ oldDragState
        oldDragState :: Maybe DragState
        oldDragState      = oldRegistry ^. UIRegistry.dragState
        widgetOverChanged :: Bool
        widgetOverChanged = oldWidgetOver /= newWidgetOver
        newWidgetOver :: Maybe WidgetId
        newWidgetOver     = case mouseEvent of
            Mouse.Event Mouse.Moved _ _ _ evWd -> (^. Mouse.widgetId) <$> evWd
            _                                  -> oldWidgetOver
        setWidgetOver :: UIRegistryHandler
        setWidgetOver state = Just $ (noUIUpdate, state & UIRegistry.widgetOver .~ newWidgetOver)

execSt (KeyboardAction keyboardEvent) oldState = (newAction, newState') where
    newAction                    = uiUpdates >> customUIUpdates
    newState                     = oldState &  Global.uiRegistry .~ newRegistry
    oldRegistry                  = oldState ^. Global.uiRegistry
    (customUIUpdates, newState') = applyHandlers (customKeyboardHandlers keyboardEvent newRegistry) newState
    (uiUpdates, newRegistry)     = UIRegistry.sequenceUpdates [ Just $ handleKeyEvents keyboardEvent ] oldRegistry
