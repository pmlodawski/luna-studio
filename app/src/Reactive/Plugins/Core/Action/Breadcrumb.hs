{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Breadcrumb where

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Node
import           JS.Bindings
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import qualified Event.Window   as Window
import           Event.Event
import           Event.WithObjects
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.Camera         as Camera
import qualified Reactive.Plugins.Core.Action.State.Global   as Global
import qualified Reactive.Plugins.Core.Action.State.Breadcrumb   as Breadcrumb
import qualified Widget.Button as Button
import           ThreeJS.Text (calculateTextWidth)
import qualified ThreeJS.Button as TButton
import qualified ThreeJS.Mesh as Mesh
import           ThreeJS.Types
import qualified ThreeJS.Scene as Scene
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import qualified JavaScript.Object as JSObject
import           ThreeJS.Registry

import           GHCJS.Prim


data Action = NewPath       { _path  :: [Text] }
            | MouseMoving   { _pos   :: Vector2 Int }
            | MousePressed  { _pos   :: Vector2 Int }
            | MouseReleased { _pos   :: Vector2 Int }
            | ButtonPressed
            | ApplyUpdates { _actions :: [IO ()] }

makeLenses ''Action

buttonHeight  = 30
buttonSpacing = 10

instance PrettyPrinter Action where
    display (NewPath path)    = "mA(" <> show path   <> ")"
    display (MouseMoving pos) = "mA(" <> display pos <> ")"


toAction :: Event Node -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button _)) = case tpe of
    Mouse.Moved     -> Just $ MouseMoving pos
    _ -> Nothing
--     Mouse.Pressed   -> case button of
--         1 -> Just $ MousePressed pos
--         _ -> Nothing
--     Mouse.Released   -> case button of
--         1 -> Just $ MouseReleased pos
--         _ -> Nothing
toAction (Window (Window.Event tpe width height)) = case tpe of
    Window.Resized  -> Just $ NewPath ["NodeLab", "demo", " by ", "New Byte Order"]
toAction _           = Nothing

createButtons :: [Text] -> Int -> ([Button.Button], Int)
createButtons path startId = (reverse buttons, nextId) where
    (buttons, (_, nextId)) = foldl button ([], (0, startId)) path
    button (xs, (offset, bid)) name = (newButton:xs, (newOffset, bid + 1)) where
       newButton = Button.Button bid label Button.Normal pos size
       width     = TButton.buttonWidth label
       pos       = Vector2 offset 0
       size      = Vector2 width buttonHeight
       newOffset = offset + width + buttonSpacing
       label     = name
       idt       = Text.pack $ show bid

instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
            Just action -> ActionUI newAction newState
            Nothing     -> ActionUI  NoAction newState
        where
        newAction = case newActionCandidate of
            MouseMoving   _  -> if focusChanged then Just $ ApplyUpdates [updateFocus] else Nothing where
                updateFocus = forM_ newButtonsFocus TButton.updateState
            MousePressed  _  -> case buttonUnderCursor of
                Just _  -> Just ButtonPressed
                Nothing -> Nothing
            MouseReleased _  -> Nothing
            NewPath _        -> Just $ ApplyUpdates [removeOldBreadcrumb, createNewBreadcrumb] where
                    removeOldBreadcrumb = forM_ (oldState ^. Global.breadcrumb . Breadcrumb.buttons) $ \b -> do
                                            uiButton <- TButton.getFromRegistry b
                                            Scene.sceneHUD `remove` uiButton
                                            TButton.removeFromRegistry b
                    createNewBreadcrumb = forM_ (newState ^. Global.breadcrumb . Breadcrumb.buttons) $ \b -> do
                                            uiButton <- TButton.buildButton b
                                            TButton.putToRegistry b uiButton
                                            Scene.sceneHUD `add` uiButton

        newState  = case newActionCandidate of
            MouseMoving  pos   -> oldState & Global.breadcrumb . Breadcrumb.buttons .~ newButtonsFocus
            NewPath      path  -> oldState & Global.breadcrumb . Breadcrumb.path    .~ path
                                           & Global.breadcrumb . Breadcrumb.nextId  .~ nextId
                                           & Global.breadcrumb . Breadcrumb.buttons .~ newButtons where
                                           (newButtons, nextId) = createButtons path startId
                                           startId = oldState ^. Global.breadcrumb . Breadcrumb.nextId
        mousePos   = oldState ^. Global.mousePos
        oldButtons = oldState ^. Global.breadcrumb . Breadcrumb.buttons
        focusChanged = any (\(o, n) -> (o ^. Button.state) /= (n ^. Button.state) ) $ oldButtons `zip` newButtonsFocus
        newButtonsFocus = updateButton <$> oldButtons where
            updateButton b = b & Button.state .~ newState where
                newState = if isOver then Button.Focused else Button.Normal
                isOver   = Button.isOver (fromIntegral <$> mousePos) b
        buttonUnderCursor = find (Button.isOver $ fromIntegral <$> mousePos) oldButtons

instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        ApplyUpdates actions -> sequence_ actions
        ButtonPressed -> do
            putStrLn "Button pressed"
