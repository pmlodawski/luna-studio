{-# LANGUAGE OverloadedStrings #-}

module Reactive.State.Sandbox where

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.UITypes
import           Object.Widget.Button (Button(..))
import qualified Object.Widget.Button as Button
import           Object.Widget.Slider (Slider(..))
import qualified Object.Widget.Slider as Slider
import           Object.Widget.Toggle (Toggle(..))
import qualified Object.Widget.Toggle as Toggle
import           Object.Widget.Chart (Chart(..))
import qualified Object.Widget.Chart as Chart
import           Object.Widget.Number (Number(..))
import qualified Object.Widget.Number as Number

data State = State { _button  :: WidgetId
                   , _slider  :: WidgetId
                   , _slider2 :: WidgetId
                   , _slider3 :: WidgetId
                   , _slider4 :: WidgetId
                   , _toggle  :: WidgetId
                   , _chart   :: WidgetId
                   , _number  :: WidgetId
                   } deriving (Show, Eq)

makeLenses ''State

instance Default State where
    def = State def def def def def def def def where

instance PrettyPrinter State where
    display (State button slider slider2 slider3 slider4 toggle chart number) =
           "dSand(button:" <> show button
        <> ", slider: "    <> show slider
        <> ", slider: "    <> show slider2
        <> ", slider: "    <> show slider3
        <> ", slider: "    <> show slider4
        <> ", toggle: "    <> show toggle
        <> ", chart : "    <> show chart
        <> ", number: "    <> show number
        <> ")"
