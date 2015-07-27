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

import           GHCJS.Prim


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

instance PrettyPrinter Action where
    display (NewPath path)  = "mA(" <> show path   <> ")"
    display (MouseMoving  pos )  = "mA(" <> display pos <> ")"


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
                                                   button (xs, (offset, bid)) name = (newButton:xs, (newOffset, bid + 1)) where
                                                       newButton = (Button.Button bid label Button.Normal pos size)
                                                       width     = TButton.buttonWidth label
                                                       pos       = (Vector2 offset 0)
                                                       size      = (Vector2 width buttonHeight)
                                                       newOffset = offset + width + buttonSpacing
                                                       label     = name
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
        UpdateFocus   -> do
            let buttons = zip [0..] $ state ^. Global.breadcrumb . Breadcrumb.buttons
            forM_ buttons updateButton where
                updateButton (i, b) = do
                    bref <- (getButton $ b ^. Button.refId) >>= return . TButton.Button . JSObject.fromJSRef
                    uniform <- JSObject.getProp "state" (TButton.unButton bref) >>= return . Attribute . JSObject.fromJSRef
                    JSObject.setProp "value" (toJSInt $ fromEnum $ b ^. Button.state) $ unAttribute uniform
        RenderButtons toRemove   -> do
            mapM_ removeButton_ toRemove
            mapM_ addButton $ state ^. Global.breadcrumb . Breadcrumb.buttons
            where
            removeButton_ b = do
                bref <- (getButton $ b ^. Button.refId) >>= return . TButton.Button . JSObject.fromJSRef
                removeButton  (b ^. Button.refId)
                mesh bref >>= remove Scene.sceneHUD
            addButton (Button.Button bid label state pos size) = do
                b <- TButton.buildButton label pos size
                putButton bid (JSObject.getJSRef $ TButton.unButton b)
                mesh b >>= add Scene.sceneHUD

        ButtonPressed -> do
            putStrLn "Button pressed"