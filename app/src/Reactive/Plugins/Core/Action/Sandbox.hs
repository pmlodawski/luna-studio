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
import           ThreeJS.Types
import qualified ThreeJS.Button                                  as UIButton
import qualified ThreeJS.Slider                                  as UISlider
import qualified ThreeJS.Scene                                   as Scene

import           GHCJS.Prim
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

data Action = InitApp
            | ApplyUpdates  { _actions :: [WidgetUIUpdate] }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "SandboxAction"

toAction (Window (Window.Event Window.Resized width height)) _ = Just $ InitApp
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
                                                 else oldState & Global.sandbox    . Sandbox.button .~ button
                                                               & Global.sandbox    . Sandbox.slider .~ slider
                                                               & Global.uiRegistry                  .~ newRegistry

            button                = (oldState ^. Global.sandbox . Sandbox.button) & Button.refId .~ buttonId
            slider                = (oldState ^. Global.sandbox . Sandbox.slider) & Slider.refId .~ sliderId
            oldRegistry           = oldState ^. Global.uiRegistry
            newRegistry           = UIRegistry.register button (UIRegistry.register slider oldRegistry)

            (buttonId:sliderId:_) = UIRegistry.generateIds 2 oldRegistry
            newAction             = if wasInited then ApplyUpdates [] else ApplyUpdates [Just $ addButton button, Just $ addSlider slider]


instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates actions) state) = sequence_ $ catMaybes actions
