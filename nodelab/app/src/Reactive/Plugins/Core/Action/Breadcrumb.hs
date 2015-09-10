{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Breadcrumb where

import           Utils.PreludePlus
import           Utils.Vector
import           Utils.CtxDynamic

import           Object.Object
import           Object.Dynamic
import           Object.Node
import           Object.UITypes
import           JS.Bindings
import           Event.Mouse                                     hiding (Event, WithObjects)
import qualified Event.Mouse                                     as Mouse
import qualified Event.Window                                    as Window
import           Event.Event
import           Event.WithObjects
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.Camera             as Camera
import qualified Reactive.Plugins.Core.Action.State.Global       as Global
import qualified Reactive.Plugins.Core.Action.State.Breadcrumb   as Breadcrumb
import qualified Reactive.Plugins.Core.Action.State.UIRegistry   as UIRegistry
import qualified Object.Widget.Button                            as Button
import           ThreeJS.Text                                    (calculateTextWidth)
import qualified ThreeJS.Widget.Button                                  as UIButton
import qualified ThreeJS.Widget.Slider                                  as UISlider
import qualified ThreeJS.Mesh                                    as Mesh
import           ThreeJS.Types
import qualified ThreeJS.Scene                                   as Scene
import qualified Data.Text.Lazy                                  as Text
import           Data.Text.Lazy                                  (Text)
import qualified JavaScript.Object                               as JSObject
import           ThreeJS.Registry                                as WidgetRegistry
import           Object.Widget                                   as Widget
import           Object.Widget.Scene (sceneInterfaceId, sceneGraphId)
import           GHCJS.Prim
import           Data.IntMap.Lazy                                (IntMap)
import qualified Data.IntMap.Lazy                                as IntMap



data Action = NewPath       { _path     :: [Text]  }
            | ApplyUpdates  { _actions  :: [IO ()] }
            | ButtonClicked { _buttonId :: Int     }

makeLenses ''Action

buttonHeight  = 30
buttonSpacing = 10

instance PrettyPrinter Action where
    display (NewPath path)      = "mA("       <> show path <> ")"
    display (ApplyUpdates u)    = "mA(upd)"
    display (ButtonClicked bid) = "mA(bclck " <> show bid  <> ")"

justIf :: Bool -> a -> Maybe a
justIf True val = Just val
justIf False  _ = Nothing

toAction (Window (Window.Event Window.Resized width height)) _ = Just $ NewPath ["NodeLab", "demo", " by ", "New Byte Order"]
toAction (Mouse (Mouse.Event Mouse.Clicked _ LeftButton _ (Just (EventWidget bid _ _)))) state = isButtonOver where
    buttonIds    = state ^. Global.breadcrumb . Breadcrumb.buttons
    isButtonOver = justIf (bid `elem` buttonIds) $ ButtonClicked bid

toAction _ _ = Nothing

createButtons :: [Text] -> [Button.Button]
createButtons path = reverse buttons where
    (buttons, _) = foldl button ([], 0) path
    button (xs, offset) name = (newButton:xs, newOffset) where
       newButton = Button.Button label Button.Normal pos size
       width     = UIButton.buttonWidth label
       pos       = Vector2 offset 0
       size      = Vector2 width buttonHeight
       newOffset = offset + width + buttonSpacing
       label     = name

addButton file = do
    uiButton <- WidgetRegistry.build (file ^. objectId) (file ^. Widget.widget)
    WidgetRegistry.register (file ^. objectId)  uiButton
    Scene.sceneHUD `add` uiButton

removeButton file = do
    uiButton <- WidgetRegistry.lookup (file ^. objectId) :: IO UIButton.Button
    Scene.sceneHUD `remove` uiButton
    WidgetRegistry.unregister (file ^. objectId)

instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
        Just action -> ActionUI newAction newState
        Nothing     -> ActionUI  NoAction newState
        where
        (newAction, newState) = (Just $ ApplyUpdates [] , oldState)
-- case newActionCandidate of
--             NewPath path        -> (action, state) where
--                     action = Just $ ApplyUpdates [removeOldBreadcrumb, createNewBreadcrumb]
--                     createNewBreadcrumb = forM_ newButtons' addButton
--                     removeOldBreadcrumb = forM_ oldButtons  removeButton
--                     newButtons   = createButtons path
--
--                     state = oldState & Global.uiRegistry                      .~ newRegistry
--                                      & Global.breadcrumb . Breadcrumb.path    .~ path
--                                      & Global.breadcrumb . Breadcrumb.buttons .~ ((^. objectId) <$> newButtons')
--                     oldButtons   =  catMaybes $ getFromRegistry <$> oldButtonIds
--                     getFromRegistry :: Int -> Maybe (WidgetFile a DisplayObject)
--                     getFromRegistry bid = case UIRegistry.lookup bid oldRegistry of
--                         Just x -> (fromCtxDynamic x) :: Maybe Button.Button
--                         _      -> Nothing
--                     oldRegistry  = oldState ^. Global.uiRegistry
--                     oldButtonIds = oldState ^. Global.breadcrumb . Breadcrumb.buttons
--                     (newButtons', newRegistry)  = UIRegistry.replaceAll sceneInterfaceId oldButtons newButtons oldRegistry
            -- ButtonClicked bid -> (Just $ ApplyUpdates [putStrLn $ "Clicked " <> (show bid)], oldState)
            -- _ -> (Nothing, oldState)

instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        ApplyUpdates actions -> sequence_ actions
