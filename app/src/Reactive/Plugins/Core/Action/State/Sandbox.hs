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
                   } deriving (Show, Eq)

makeLenses ''State

instance Default State where
    def = State button slider where
        button = Button 0 "Click me!" Button.Normal (Vector2 100 100) (Vector2 100 50)
        slider = Slider 1 (Vector2 100 200) (Vector2 100 50) 0.0 100.0 30.0

instance PrettyPrinter State where
    display (State button slider) =
           "dSand(button:" <> show button
        <> ", slider: "    <> show slider
        <> ")"