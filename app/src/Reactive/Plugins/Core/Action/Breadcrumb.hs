{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Breadcrumb where

import           Utils.PreludePlus
import           Utils.Vector

import qualified JS.Widget.Button as JS
import           Object.Object
import           Object.Node
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


data Action = NewPath     { _path  :: [Text] }
            | MouseMoving { _pos   :: Vector2 Int }
            | UpdateFocus
              deriving (Eq, Show)

makeLenses ''Action

buttonHeight  = 30
buttonSpacing = 10
buttonPadding = 10

instance PrettyPrinter Action where
    display (NewPath path)  = "mA(" <> show path   <> ")"
    display (MouseMoving  pos )  = "mA(" <> display pos <> ")"


toAction :: Event Node -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos _ _)) = case tpe of
    Mouse.Moved     -> Just $ MouseMoving pos
    _               -> Nothing
toAction (Window (Window.Event tpe width height)) = case tpe of
    Window.Resized  -> Just $ NewPath ["Foo", "Bar", "to jest dluga nazwa", "Ala123"]
toAction _           = Nothing

instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
            Just action -> ActionUI newAction newState
            Nothing     -> ActionUI  NoAction newState
        where
        newAction = case newActionCandidate of
            MouseMoving pos  -> if focusChanged then Just UpdateFocus else Nothing
            _                -> Just newActionCandidate
        newState  = case newActionCandidate of
            MouseMoving  pos   -> oldState &  Global.breadcrumb . Breadcrumb.buttons .~ newButtons where
            NewPath      path  -> oldState & Global.breadcrumb . Breadcrumb.path    .~ path
                                      & Global.breadcrumb . Breadcrumb.buttons .~ (reverse buttons) where
                                          (buttons, _) = foldl button ([], 0) path where
                                          button (xs, left) name = ((Button.Button name Button.Normal (Vector2 left 0) (Vector2 width buttonHeight)):xs, left + width + buttonSpacing ) where
                                              width = (JS.calculateTextWidth name) + 2*buttonPadding
        mousePos   = oldState ^. Global.mousePos
        oldButtons = oldState ^. Global.breadcrumb . Breadcrumb.buttons
        newButtons = updateButton <$> oldButtons where
            updateButton b = b & Button.state .~ newState where
                newState = if isOver then Button.Focused else Button.Normal
                isOver   = Button.isOver b (fromIntegral <$> mousePos)
        focusChanged = any (\(o, n) -> (o ^. Button.state) /= (n ^. Button.state) ) $ oldButtons `zip` newButtons

instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        UpdateFocus   -> do
            let buttons = zip [0..] $ state ^. Global.breadcrumb . Breadcrumb.buttons
            forM_ buttons (\(i, b) -> JS.setButtonState i (fromEnum $ b ^. Button.state))
        NewPath path  -> do
            JS.clearBreadcrumb
            let addButton b = do
                jsb <- Button.toJSButton b
                JS.addBreadcrumb jsb
            mapM_ addButton $ state ^. Global.breadcrumb . Breadcrumb.buttons
            putStrLn "DUPA"
