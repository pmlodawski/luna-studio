{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Sandbox where

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Widget
import           Object.Node
import           Event.Mouse    hiding      (Event, WithObjects)
import qualified Event.Mouse    as Mouse
import qualified Event.Window   as Window
import           Event.Event
import           Event.WithObjects
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global       as Global
import qualified Reactive.Plugins.Core.Action.State.Sandbox      as Sandbox
import           Reactive.Plugins.Core.Action.State.UIRegistry   (WidgetMap)
import qualified Reactive.Plugins.Core.Action.State.UIRegistry   as UIRegistry
import qualified Object.Widget.Button                            as Button
import qualified Object.Widget.Slider                            as Slider
import qualified Object.Widget.Toggle                            as Toggle
import qualified Object.Widget.Chart                             as Chart
import           ThreeJS.Types
import qualified ThreeJS.Button                                  as UIButton
import qualified ThreeJS.Slider                                  as UISlider
import qualified ThreeJS.Toggle                                  as UIToggle
import qualified Dimple.Render                                   as UIChart
import qualified ThreeJS.Scene                                   as Scene

import           GHCJS.Prim
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

data Action = InitApp
            | ApplyUpdates  { _actions  :: [WidgetUIUpdate] }
            | WidgetClicked { _buttonId :: WidgetId }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "SandboxAction"

toAction (Window (Window.Event Window.Resized width height)) _ = Just $ InitApp
toAction (Mouse (Mouse.Event Mouse.Clicked _ Mouse.LeftButton _ (Just (EventWidget bid _ _)))) state = Just $ WidgetClicked bid
toAction _ _        = Nothing

sandboxScene = Scene.scene

addButton b = do
    uiButton <- UIButton.buildButton b
    UIButton.putToRegistry b uiButton
    sandboxScene `add` uiButton

addSlider b = do
    slider <- UISlider.buildSlider b
    UISlider.putToRegistry b slider
    sandboxScene `add` slider

addToggle b = do
    toggle <- UIToggle.buildToggle b
    UIToggle.putToRegistry b toggle
    sandboxScene `add` toggle

addChart b = do
    UIChart.displayChart b

removeButton b = do
    uiButton <- UIButton.getFromRegistry b
    sandboxScene `remove` uiButton
    UIButton.removeFromRegistry b

removeSlider b = do
    uiButton <- UISlider.getFromRegistry b
    sandboxScene `remove` uiButton
    UISlider.removeFromRegistry b

instance ActionStateUpdater Action where
    execSt InitApp oldState = ActionUI  newAction newState
             where
             wasInited             = oldState ^. Global.sandbox . Sandbox.button . Button.refId /= 0
             newState              = if wasInited then oldState
                                                  else oldState & Global.sandbox    . Sandbox.button  .~ button
                                                                & Global.sandbox    . Sandbox.slider  .~ slider
                                                                & Global.sandbox    . Sandbox.slider2 .~ slider2
                                                                & Global.sandbox    . Sandbox.slider3 .~ slider3
                                                                & Global.sandbox    . Sandbox.slider4 .~ slider4
                                                                & Global.sandbox    . Sandbox.toggle  .~ toggle
                                                                & Global.sandbox    . Sandbox.chart   .~ chart
                                                                & Global.uiRegistry                   .~ newRegistry

             button                = (oldState ^. Global.sandbox . Sandbox.button)  & Button.refId .~ buttonId
             slider                = (oldState ^. Global.sandbox . Sandbox.slider)  & Slider.refId .~ sliderId
             slider2               = (oldState ^. Global.sandbox . Sandbox.slider2) & Slider.refId .~ sliderId2
             slider3               = (oldState ^. Global.sandbox . Sandbox.slider3) & Slider.refId .~ sliderId3
             slider4               = (oldState ^. Global.sandbox . Sandbox.slider4) & Slider.refId .~ sliderId4
             toggle                = (oldState ^. Global.sandbox . Sandbox.toggle)  & Toggle.refId .~ toggleId
             chart                 = (oldState ^. Global.sandbox . Sandbox.chart )  & Chart.refId .~ chartId
             oldRegistry           = oldState ^. Global.uiRegistry
             newRegistry           = UIRegistry.register button $ UIRegistry.register slider $ UIRegistry.register slider2 $ UIRegistry.register slider3 $ UIRegistry.register slider4 $ UIRegistry.register toggle $ UIRegistry.register chart oldRegistry

             (buttonId:sliderId:sliderId2:sliderId3:sliderId4:toggleId:chartId:_) = UIRegistry.generateIds 7 oldRegistry
             newAction             = if wasInited then ApplyUpdates [] else ApplyUpdates [Just $ addButton button, Just $ addSlider slider, Just $ addSlider slider2, Just $ addSlider slider3, Just $ addSlider slider4, Just $ addToggle toggle, Just $ addChart chart]
    execSt (WidgetClicked bid) oldState = ActionUI  newAction newState where
        oldRegistry           = oldState ^. Global.uiRegistry
        widget                = UIRegistry.lookup bid oldRegistry
        newRegistry           = oldRegistry -- tu mozna np. zrobis UIRegistry.update, etc.
        newState              = oldState & Global.uiRegistry .~ newRegistry
        wasHelloButtonClicked = bid == oldState ^. Global.sandbox . Sandbox.button . Button.refId
        newAction             = if wasHelloButtonClicked then ApplyUpdates [Just $ putStrLn "HelloWorld"]
                                                            -- tu tablica Maybe (IO ()), np aktualizacja stanu widgeta
                                                         else ApplyUpdates []

instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates actions) state) = sequence_ $ catMaybes actions
