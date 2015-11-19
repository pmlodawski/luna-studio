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
import           Reactive.Commands.Command   (Command, execCommand)
import           Object.Widget.Port   ()
import           UI.Handlers   (widgetHandlers)

-- ifJustThen :: Monad m => Maybe a -> m b
ifJustThen (Just m) f = f m
ifJustThen _ _ = return ()

toAction :: Event -> Maybe (Command Global.State ())
toAction (Mouse jsState event) = Just $ do
    handleMouseOverOut jsState event
    handleMouseDrag    jsState event
    handleMouseGeneric jsState event
toAction (Keyboard jsState event) = Just $ handleKeyboardGeneric jsState event
toAction _ = Nothing

handleMouseGeneric :: JSState -> Mouse.RawEvent -> Command Global.State ()
handleMouseGeneric jsState event@(Mouse.Event eventType absPos button keymods (Just (EventWidget widgetId mat scene))) = do
    file <- zoom Global.uiRegistry $ UIRegistry.lookupM widgetId
    ifJustThen file $ \file -> do
        camera <- use $ Global.camera . Camera.camera
        let pos = absPosToRel scene camera mat (fromIntegral <$> absPos)
        let updatedEvent = event & Mouse.position .~ pos
        let handlers = widgetHandlers (file ^. widget)
        case eventType of
            Mouse.Moved       -> (handlers ^. mouseMove)     updatedEvent jsState widgetId
            Mouse.Pressed     -> (handlers ^. mousePressed)  updatedEvent jsState widgetId
            Mouse.Released    -> (handlers ^. mouseReleased) updatedEvent jsState widgetId
            Mouse.Clicked     -> (handlers ^. click)         updatedEvent jsState widgetId
            Mouse.DblClicked  -> (handlers ^. dblClick)      updatedEvent jsState widgetId
            otherwise         -> return ()
handleMouseGeneric _ _ = return ()

runOverHandler :: JSState -> (UIHandlers Global.State -> (JSState -> WidgetId -> Command Global.State ())) -> Command Global.State ()
runOverHandler jsState handler = do
    widgetOver <- use $  Global.uiRegistry . UIRegistry.widgetOver

    ifJustThen widgetOver $ \widgetOver -> do
        file <- zoom Global.uiRegistry $ UIRegistry.lookupM widgetOver
        ifJustThen file $ \file -> do
            let handlers = widgetHandlers (file ^. widget)
            (handler handlers) jsState widgetOver

handleMouseOut :: JSState -> Command Global.State ()
handleMouseOut jsState = do
    runOverHandler jsState $ view mouseOut
    Global.uiRegistry . UIRegistry.widgetOver .= Nothing

handleMouseOverOut ::  JSState -> Mouse.RawEvent -> Command Global.State ()
handleMouseOverOut jsState (Mouse.Event Mouse.Moved absPos button keymods Nothing) = handleMouseOut jsState
handleMouseOverOut jsState (Mouse.Event Mouse.Moved absPos button keymods (Just (EventWidget widgetId mat scene))) = do
    widgetOver <- use $ Global.uiRegistry . UIRegistry.widgetOver
    when (widgetOver /= Just widgetId) $ do
        handleMouseOut jsState
        Global.uiRegistry . UIRegistry.widgetOver ?= widgetId
        runOverHandler jsState $ view mouseOver

handleMouseOverOut _ _ = return ()

absPosToRel :: SceneType -> Camera.Camera -> [Double] -> Vector2 Double -> Vector2 Double
absPosToRel HUD       _      mat pos = Widget.sceneToLocal pos          mat
absPosToRel Workspace camera mat pos = Widget.sceneToLocal workspacePos mat where
    workspacePos = Camera.screenToWorkspace camera (round <$> pos)

handleKeyboardGeneric :: JSState -> Keyboard.Event -> Command Global.State ()
handleKeyboardGeneric jsState (Keyboard.Event eventType ch mods) = do
    focusedWidget <- use $ Global.uiRegistry . UIRegistry.focusedWidget
    ifJustThen focusedWidget $ \focusedWidget -> do
        file <- zoom Global.uiRegistry $ UIRegistry.lookupM focusedWidget
        ifJustThen file $ \file -> do
            let handlers = widgetHandlers (file ^. widget)
            case eventType of
                Keyboard.Up       -> (handlers ^. keyUp)       ch mods jsState focusedWidget
                Keyboard.Down     -> (handlers ^. keyDown)     ch mods jsState focusedWidget
                Keyboard.Press    -> (handlers ^. keyPressed)  ch mods jsState focusedWidget

handleMouseDrag ::  JSState -> Mouse.RawEvent -> Command Global.State ()
handleMouseDrag jsState (Mouse.Event Mouse.Moved absPos _ keymods _) = do
    dragState <- use $ Global.uiRegistry . UIRegistry.dragState

    ifJustThen dragState $ \dragState -> do
        file <- zoom Global.uiRegistry $ UIRegistry.lookupM $ dragState ^. Widget.widgetId
        camera <- use $ Global.camera . Camera.camera
        let pos = absPosToRel (dragState ^. Widget.scene) camera (dragState ^. Widget.widgetMatrix) (fromIntegral <$> absPos)

        zoom (Global.uiRegistry . UIRegistry.dragState . _Just) $ do
            Widget.keyMods .= keymods
            prevPos' <- use Widget.previousPos
            Widget.previousPos .= prevPos'
            Widget.currentPos .= pos

        ifJustThen file $ \file -> do
            let handlers = widgetHandlers (file ^. widget)
            (handlers ^. dragMove) dragState jsState (dragState ^. Widget.widgetId)


handleMouseDrag jsState (Mouse.Event Mouse.Released _ _ _ _) = do
    dragState <- use $ Global.uiRegistry . UIRegistry.dragState

    ifJustThen dragState $ \dragState -> do
        file <- zoom Global.uiRegistry $ UIRegistry.lookupM $ dragState ^. widgetId
        ifJustThen file $ \file -> do
            let handlers = widgetHandlers (file ^. widget)
            (handlers ^. dragEnd) dragState jsState (dragState ^. widgetId)

        Global.uiRegistry . UIRegistry.dragState .= Nothing
handleMouseDrag _ _ = return ()
