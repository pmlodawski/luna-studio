{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.State.Sandbox where

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Widget.Button (Button(..))
import qualified Object.Widget.Button as Button
import           Object.Widget.Slider (Slider(..))
import qualified Object.Widget.Slider as Slider

data State = State { _button :: Button
                   , _slider :: Slider
                   , _slider2 :: Slider
                   , _slider3 :: Slider
                   , _slider4 :: Slider
                   } deriving (Show, Eq)

makeLenses ''State

instance Default State where
    def = State button slider slider2 slider3 slider4 where
        button  = Button 0 "Click me!" Button.Normal (Vector2 100 100) (Vector2 100 50)
        slider  = Slider 1 (Vector2 100 200) (Vector2 200 25) "Cutoff" 100.0 20000.0 1000.0
        slider2 = Slider 2 (Vector2 100 230) (Vector2 200 25) "Resonance" 0.0 100.0 30.0
        slider3 = Slider 3 (Vector2 100 260) (Vector2 200 25) "Noise" 0.0 1.0 0.1
        slider4 = Slider 4 (Vector2 100 290) (Vector2 200 25) "Gamma" 0.0000001 0.000001 0.0000005

instance PrettyPrinter State where
    display (State button slider slider2 slider3 slider4) =
           "dSand(button:" <> show button
        <> ", slider: "    <> show slider
        <> ", slider: "    <> show slider2
        <> ", slider: "    <> show slider3
        <> ", slider: "    <> show slider4
        <> ")"