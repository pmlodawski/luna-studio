{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Sandbox where

import           Utils.PreludePlus
import           Utils.Vector

import           JS.Config

import           Object.Widget
import           Object.Node
import           Object.UITypes

import           Event.Mouse    hiding      (Event, WithObjects, widget)
import qualified Event.Mouse    as Mouse
import qualified Event.Window   as Window
import           Event.Event
import           Event.WithObjects

import           Reactive.Plugins.Core.Action
import qualified Reactive.Plugins.Core.Action.State.Global       as Global
import qualified Reactive.Plugins.Core.Action.State.Sandbox      as Sandbox
import           Reactive.Plugins.Core.Action.State.UIRegistry   (WidgetMap, sceneInterfaceId, sceneGraphId)
import qualified Reactive.Plugins.Core.Action.State.UIRegistry   as UIRegistry
import           Reactive.Plugins.Core.Action.Commands.Command   (performIO, runCommand)

import qualified Object.Widget.Button                            as Button
import qualified Object.Widget.Slider                            as Slider
import qualified Object.Widget.Toggle                            as Toggle
import qualified Object.Widget.Number                            as Number
import qualified Object.Widget.Chart                             as Chart

import           Object.Widget.Button                            (Button(..))
import           Object.Widget.Slider                            (Slider(..))
import           Object.Widget.Toggle                            (Toggle(..))
import           Object.Widget.Number                            (Number(..))
import           Object.Widget.Chart                             (Chart(..))

import           ThreeJS.Types
import qualified ThreeJS.Registry                                as JSRegistry
import qualified ThreeJS.Widget.Button                           as UIButton
import qualified ThreeJS.Widget.Slider                           as UISlider
import qualified ThreeJS.Widget.Toggle                           as UIToggle
import qualified ThreeJS.Widget.Number                           as UINumber
import qualified ThreeJS.Scene                                   as Scene
import qualified Dimple.Render                                   as UIChart
import qualified Control.Monad.State                             as MState

import qualified BatchConnector.Commands as BatchCmd
import qualified Batch.Workspace         as Workspace

data Action = InitApp
            | ApplyUpdates  { _actions  :: WidgetUIUpdate }
            | WidgetClicked { _buttonId :: WidgetId }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "SandboxAction"

toAction (Window (Window.Event Window.Resized width height)) _ = Just $ InitApp
toAction (Mouse (Mouse.Event Mouse.Clicked _ LeftButton _ (Just (EventWidget bid _ _)))) state = Just $ WidgetClicked bid
toAction _ _        = Nothing

sandboxScene = Scene.scene

addWidget b = do
    w <- JSRegistry.build (b ^. objectId) (b ^. widget)
    JSRegistry.register (b ^. objectId) w
    sandboxScene `add` w

addChart b = do
    UIChart.displayChart (b ^. widget)


instance ActionStateUpdater Action where
    execSt InitApp oldState = ActionUI  newAction newState
            where
            wasInited             = oldState ^. Global.sandbox . Sandbox.button  /= 0
            newState              = if wasInited || not widgetSandboxEnabled then oldState
                                                 else oldState & Global.sandbox    . Sandbox.button  .~ (button  ^. objectId)
                                                               & Global.sandbox    . Sandbox.slider  .~ (slider  ^. objectId)
                                                               & Global.sandbox    . Sandbox.slider2 .~ (slider2 ^. objectId)
                                                               & Global.sandbox    . Sandbox.slider3 .~ (slider3 ^. objectId)
                                                               & Global.sandbox    . Sandbox.slider4 .~ (slider4 ^. objectId)
                                                               & Global.sandbox    . Sandbox.toggle  .~ (toggle  ^. objectId)
                                                               & Global.sandbox    . Sandbox.chart   .~ (chart   ^. objectId)
                                                               & Global.sandbox    . Sandbox.number  .~ (number  ^. objectId)
                                                               & Global.uiRegistry                   .~ newRegistry

            oldRegistry     = oldState ^. Global.uiRegistry
            registerWidgets = do
                button  <- UIRegistry.registerM sceneGraphId
                           (Button "Save" Button.Normal (Vector2 100 100) (Vector2 100 50))
                           def
                performIO $ addWidget button

                slider  <- UIRegistry.registerM sceneGraphId (Slider (Vector2 100 200) (Vector2 200  25) "Cutoff"    100      20000        0.1 :: Slider Int) def
                performIO $ addWidget slider

                slider2 <- UIRegistry.registerM sceneGraphId (Slider (Vector2 100 230) (Vector2 200  25) "Resonance"   0.0      100.0      0.3 :: Slider Double) def
                performIO $ addWidget slider2

                slider3 <- UIRegistry.registerM sceneGraphId (Slider (Vector2 100 260) (Vector2 200  25) "Noise"       0.0        1.0      0.1 :: Slider Double) def
                performIO $ addWidget slider3

                slider4 <- UIRegistry.registerM sceneGraphId (Slider (Vector2 100 290) (Vector2 200  25) "Gamma"       0.0000001  0.000001 0.9 :: Slider Double) def
                performIO $ addWidget slider4

                toggle  <- UIRegistry.registerM sceneGraphId (Toggle (Vector2 100 320) (Vector2 200  25) "Inverse" False) def
                performIO $ addWidget toggle

                chart   <- UIRegistry.registerM sceneGraphId (Chart  (Vector2 100 380) (Vector2 300 200) Chart.Bar "Brand" Chart.Category "Unit Sales" Chart.Linear) def
                performIO $ addChart chart

                number  <- UIRegistry.registerM sceneGraphId (Number (Vector2 100 170) (Vector2 200  25) "Count" 4096 :: Number Int) def
                performIO $ addWidget number

                return (button, slider, slider2, slider3, slider4, toggle, chart, number)

            ((button, slider, slider2, slider3, slider4, toggle, chart, number), actions, newRegistry) = runCommand registerWidgets oldRegistry

            newAction             = if wasInited || not widgetSandboxEnabled then ApplyUpdates (return ()) else ApplyUpdates actions
    execSt (WidgetClicked bid) oldState = ActionUI  newAction newState where
        oldRegistry           = oldState ^. Global.uiRegistry
        widget                = UIRegistry.lookup bid oldRegistry
        newRegistry           = oldRegistry
        newState              = oldState & Global.uiRegistry .~ newRegistry
        wasSaveButtonClicked  = bid == oldState ^. Global.sandbox . Sandbox.button
        newAction             = if wasSaveButtonClicked then ApplyUpdates $ BatchCmd.storeProject (oldState ^. Global.workspace . Workspace.project)
                                                        else ApplyUpdates (return ())

instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates actions) state) = actions
