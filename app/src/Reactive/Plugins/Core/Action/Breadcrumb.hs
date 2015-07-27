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
import ThreeJS.Types
import qualified ThreeJS.Scene as Scene
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)



data Action = NewPath     { _path  :: [Text] }
            | MouseMoving { _pos   :: Vector2 Int }
            | MousePressed  { _pos   :: Vector2 Int }
            | MouseReleased { _pos   :: Vector2 Int }
            | UpdateFocus
            | ButtonPressed
            | RenderButtons { _toRemove :: [Button.Button] }
              deriving (Eq, Show)

makeLenses ''Action

buttonHeight  = 30
buttonSpacing = 10
buttonPadding = 20

instance PrettyPrinter Action where
    display (NewPath path)  = "mA(" <> show path   <> ")"
    display (MouseMoving  pos )  = "mA(" <> display pos <> ")"


toAction :: Event Node -> Maybe Action
-- toAction (Mouse (Mouse.Event tpe pos button _)) = case tpe of
--     Mouse.Moved     -> Just $ MouseMoving pos
--     Mouse.Pressed   -> case button of
--         1 -> Just $ MousePressed pos
--         _ -> Nothing
--     Mouse.Released   -> case button of
--         1 -> Just $ MouseReleased pos
--         _ -> Nothing
toAction (Window (Window.Event tpe width height)) = case tpe of
    Window.Resized  -> Just $ NewPath ["Foo", "Bar", "to jest dluga nazwa", "Ala123"]
toAction _           = Nothing

instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
            Just action -> ActionUI newAction newState
            Nothing     -> ActionUI  NoAction newState
        where
        newAction = case newActionCandidate of
            MouseMoving   _  -> if focusChanged then Just UpdateFocus else Nothing
            MousePressed  _  -> case buttonUnderCursor of
                Just _  -> Just ButtonPressed
                Nothing -> Nothing
            MouseReleased _  -> Nothing
            NewPath _        -> Just $ RenderButtons oldButtons
        newState  = case newActionCandidate of
            MouseMoving  pos   -> oldState & Global.breadcrumb . Breadcrumb.buttons .~ newButtonsFocus
            NewPath      path  -> oldState & Global.breadcrumb . Breadcrumb.path    .~ path
                                           & Global.breadcrumb . Breadcrumb.nextId  .~ nextId
                                           & Global.breadcrumb . Breadcrumb.buttons .~ (reverse buttons) where
                                               (buttons, (_, nextId)) = foldl button ([], (0, startId)) path where
                                                   button (xs, (offset, bid)) name = (newButton:xs, (newOffset, bid+1)) where
                                                       newButton = (Button.Button bid label Button.Normal pos size)
                                                       width     = (ThreeJS.Text.calculateTextWidth name) + 2*buttonPadding
                                                       pos       = (Vector2 offset 0)
                                                       size      = (Vector2 width buttonHeight)
                                                       newOffset = offset + width + buttonSpacing
                                                       label     = name <> idt
                                                       idt :: Text
                                                       idt = Text.pack $ show bid
                                                   startId = oldState ^. Global.breadcrumb . Breadcrumb.nextId
                                               numButtons = length buttons
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
        UpdateFocus   -> putStrLn "-" -- do
--             let buttons = zip [0..] $ state ^. Global.breadcrumb . Breadcrumb.buttons
--             forM_ buttons (\(i, b) -> JS.setButtonState i (fromEnum $ b ^. Button.state))
        RenderButtons toRemove   -> do
            let removeButton_ b = do
                let bid = b ^. Button.refId
                b <- (getButton $ b ^. Button.refId) :: IO Mesh
                removeButton bid
                Scene.sceneHUD `remove` b

            mapM_ removeButton_ toRemove

            let addButton (Button.Button bid label state pos size) = do
                b <- TButton.buildButton label pos size
                putButton bid (mesh b)
                Scene.sceneHUD `add` (mesh b)

            mapM_ addButton $ state ^. Global.breadcrumb . Breadcrumb.buttons
        ButtonPressed -> do
            putStrLn "Button pressed"