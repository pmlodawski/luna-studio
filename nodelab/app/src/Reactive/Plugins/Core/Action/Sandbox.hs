{-# LANGUAGE OverloadedStrings #-}
module Reactive.Plugins.Core.Action.Sandbox where

import           Utils.PreludePlus hiding (Choice)
import           Utils.Vector

import           Event.Event      (Event(..))
import qualified Event.Keyboard as Keyboard
import qualified Reactive.State.Global           as Global
import           Reactive.Commands.Command       (Command, performIO)

import           Reactive.State.UIRegistry     (sceneInterfaceId, sceneGraphId, addHandler)
import qualified Reactive.State.UIRegistry     as UIRegistry
import qualified Reactive.Commands.UIRegistry  as UICmd

import qualified Object.Widget.Number.Discrete as DiscreteNumber
import qualified UI.Handlers.Number.Discrete   as DiscreteNumber
import           UI.Widget.Number.Discrete ()

import qualified Object.Widget.Number.Continuous as ContinuousNumber
import qualified UI.Handlers.Number.Continuous   as ContinuousNumber
import           UI.Widget.Number.Continuous ()

import qualified Object.Widget.Slider.Discrete as DiscreteSlider
import qualified UI.Handlers.Slider.Discrete   as DiscreteSlider
import           UI.Widget.Slider.Discrete ()

import qualified Object.Widget.Slider.Continuous as ContinuousSlider
import qualified UI.Handlers.Slider.Continuous   as ContinuousSlider
import           UI.Widget.Slider.Continuous ()

import qualified Object.Widget.Toggle as Toggle
import qualified UI.Handlers.Toggle   as Toggle
import           UI.Widget.Toggle ()

import qualified Object.Widget.TextBox as TextBox
import qualified UI.Handlers.TextBox as TextBox
import           UI.Widget.TextBox ()

import qualified Object.Widget.Group as Group
import           UI.Widget.Group ()

import           Object.Widget.Choice.RadioButton  (RadioButton(..))
import qualified Object.Widget.Choice.RadioButton as RadioButton
import qualified UI.Handlers.Choice.RadioButton   as RadioButton
import           UI.Widget.Choice.RadioButton ()

import           Object.Widget.Choice  (Choice(..))
import qualified Object.Widget.Choice  as Choice
import qualified UI.Handlers.Choice    as Choice
import qualified UI.Command.Choice     as Choice
import           UI.Widget.Choice ()

import           UI.Layout as Layout
import           Object.UITypes
import           Object.Widget
import           Reactive.State.Global (inRegistry)
import qualified Data.HMap.Lazy as HMap
import           Data.HMap.Lazy (HTMap)


toAction :: Event -> Maybe (Command Global.State ())
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\112' _)) = Just $ Global.inRegistry $ do
    performIO $ putStrLn "show sandbox"
    let widget = Group.create
    parent <- UICmd.register sceneGraphId widget def


    let widget = DiscreteNumber.create (Vector2 180 20) "DiscreteNumber" 42
    UICmd.register_ parent widget def

    let widget = ContinuousNumber.create (Vector2 180 20) "ContinuousNumber" 42.42
    UICmd.register_ parent widget def

    let widget = ContinuousSlider.create (Vector2 180 20) "ContinuousSlider" (-2.0) 5.0 3.0
    UICmd.register_ parent widget def

    let widget = DiscreteSlider.create (Vector2 180 20) "DiscreteSlider" (-20) 80 30
    UICmd.register_ parent widget def

    let widget = DiscreteSlider.create (Vector2 180 20) "DiscreteSlider" 3 8 4
    UICmd.register_ parent widget def

    let widget = Toggle.create (Vector2 180 20) "Toggle" True
    UICmd.register_ parent widget def

    let widget = TextBox.create (Vector2 180 20) "TextBox" "foo"
    UICmd.register_ parent widget def

    let widget = Choice.create (Vector2 180 20) "Choice" ["Foo", "Bar", "Baz"] 0
    Choice.makeChoice parent widget

    Layout.verticalLayout 10.0 parent

    return ()

toAction _  = Nothing



