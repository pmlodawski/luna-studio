{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Breadcrumb where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.CtxDynamic

import           Object.Object
import           Object.Dynamic
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
import qualified Reactive.Plugins.Core.Action.State.UIRegistry   as UIRegistry
import qualified Object.Widget.Button as Button
import           ThreeJS.Text (calculateTextWidth)
import qualified ThreeJS.Button as UIButton
import qualified ThreeJS.Mesh as Mesh
import           ThreeJS.Types
import qualified ThreeJS.Scene as Scene
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import qualified JavaScript.Object as JSObject
import           ThreeJS.Registry
import           Object.Widget as Widget
import           GHCJS.Prim
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap



data Action = NewPath       { _path     :: [Text] }
            | ApplyUpdates  { _actions  :: [IO ()] }
            | ButtonClicked { _buttonId :: Int }

makeLenses ''Action

buttonHeight  = 30
buttonSpacing = 10

instance PrettyPrinter Action where
    display (NewPath path)    = "mA(" <> show path <> ")"
    display (ApplyUpdates u)  = "mA(upd)"
    display (ButtonClicked bid)  = "mA(bclck " <> show bid <> ")"

justIf :: Bool -> a -> Maybe a
justIf True val = Just val
justIf False  _ = Nothing

toAction (Window (Window.Event Window.Resized width height)) _ = Just $ NewPath ["NodeLab", "demo", " by ", "New Byte Order"]
toAction (Mouse (Mouse.Event Mouse.Clicked _ Mouse.LeftButton _ (Just bid) _)) state = isButtonOver where
    buttonIds  = state ^. Global.breadcrumb . Breadcrumb.buttons
    isButtonOver = justIf (bid `elem` buttonIds) $ ButtonClicked bid

toAction _ _ = Nothing

createButtons :: [Text] -> Int -> ([Button.Button], Int)
createButtons path startId = (reverse buttons, nextId) where
    (buttons, (_, nextId)) = foldl button ([], (0, startId)) path
    button (xs, (offset, bid)) name = (newButton:xs, (newOffset, bid + 1)) where
       newButton = Button.Button bid label Button.Normal pos size
       width     = UIButton.buttonWidth label
       pos       = Vector2 offset 0
       size      = Vector2 width buttonHeight
       newOffset = offset + width + buttonSpacing
       label     = name

addButton b = do
    uiButton <- UIButton.buildButton b
    UIButton.putToRegistry b uiButton
    Scene.sceneHUD `add` uiButton

removeButton b = do
    uiButton <- UIButton.getFromRegistry b
    Scene.sceneHUD `remove` uiButton
    UIButton.removeFromRegistry b

instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI  NoAction newState
        where
        (newAction, newState) = case newActionCandidate of
            NewPath path        -> (action, state) where
                    action = Just $ ApplyUpdates [removeOldBreadcrumb, createNewBreadcrumb]
                    createNewBreadcrumb = forM_ newButtons addButton
                    removeOldBreadcrumb = forM_ oldButtons removeButton

                    state = oldState & Global.uiRegistry . UIRegistry.nextId  .~ nextId
                                     & Global.uiRegistry . UIRegistry.widgets .~ newWidgets
                                     & Global.breadcrumb . Breadcrumb.path    .~ path
                                     & Global.breadcrumb . Breadcrumb.buttons .~ ((^. Button.refId) <$> newButtons)
                    (newButtons, nextId) = createButtons path startId
                    oldButtons =  catMaybes $ getFromRegistry <$> oldButtonIds
                    getFromRegistry :: Int -> Maybe Button.Button
                    getFromRegistry bid = fromCtxDynamic $ oldRegistry IntMap.! bid
                    oldRegistry = (oldState ^. Global.uiRegistry . UIRegistry.widgets)
                    oldButtonIds = oldState ^. Global.breadcrumb . Breadcrumb.buttons
                    newWidgets = UIRegistry.replaceAll oldRegistry oldButtons newButtons
                    startId = oldState ^. Global.uiRegistry .UIRegistry.nextId
            ButtonClicked bid -> (Just $ ApplyUpdates [putStrLn $ show bid], oldState)
            _ -> (Nothing, oldState)

instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        ApplyUpdates actions -> sequence_ actions
