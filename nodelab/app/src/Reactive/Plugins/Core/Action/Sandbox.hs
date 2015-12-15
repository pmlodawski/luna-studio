{-# LANGUAGE OverloadedStrings #-}
module Reactive.Plugins.Core.Action.Sandbox where

import           Utils.PreludePlus                hiding (Choice)
import           Utils.Vector

import           Event.Event                      (Event (..))
import qualified Event.Keyboard                   as Keyboard
import           Reactive.Commands.Command        (Command, performIO)
import qualified Reactive.State.Global            as Global

import qualified Reactive.Commands.UIRegistry     as UICmd
import           Reactive.State.UIRegistry        (addHandler, sceneGraphId,
                                                   sceneInterfaceId)
import qualified Reactive.State.UIRegistry        as UIRegistry

import           UI.Instances

import qualified Object.Widget.Group              as Group
import qualified Object.Widget.LabeledTextBox     as LabeledTextBox
import qualified Object.Widget.List               as List
import qualified Object.Widget.Number.Continuous  as ContinuousNumber
import qualified Object.Widget.Number.Discrete    as DiscreteNumber
import qualified Object.Widget.Slider.Continuous  as ContinuousSlider
import qualified Object.Widget.Slider.Discrete    as DiscreteSlider
import qualified Object.Widget.TextBox            as TextBox
import qualified Object.Widget.Toggle             as Toggle

import           Object.Widget.Choice.RadioButton (RadioButton (..))
import qualified Object.Widget.Choice.RadioButton as RadioButton

import           Object.Widget.Choice             (Choice (..))
import qualified Object.Widget.Choice             as Choice

import           Data.HMap.Lazy                   (HTMap)
import qualified Data.HMap.Lazy                   as HMap
import           Object.LunaValue
import           Object.LunaValue.Instances
import           Object.UITypes
import           Object.Widget
import           Reactive.State.Global            (inRegistry)
import           UI.Layout                        as Layout


toAction :: Event -> Maybe (Command Global.State ())
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\112' _)) = Just $ Global.inRegistry $ do
    performIO $ putStrLn "show sandbox"
    let widget = Group.create
    parent <- UICmd.register sceneGraphId widget def

    let widget = DiscreteNumber.create (Vector2 180 20) "Discrete" 42
    UICmd.register parent widget def

    let widget = ContinuousNumber.create (Vector2 180 20) "Continuous" 42.42
    UICmd.register parent widget def

    let widget = ContinuousSlider.create (Vector2 180 20) "ContinuousSlider" (-2.0) 5.0 3.0
    UICmd.register parent widget def

    let widget = DiscreteSlider.create (Vector2 180 20) "DiscreteSlider" (-15) 80 30
    UICmd.register parent widget def

    let widget = DiscreteSlider.create (Vector2 180 20) "DiscreteSlider" 3 8 4
    UICmd.register parent widget def

    let widget = Toggle.create (Vector2 180 20) "Toggle" True
    UICmd.register_ parent widget def

    let widget = LabeledTextBox.create (Vector2 180 20) "Textbox" "foo"
    UICmd.register_ parent widget def

    let widget = Choice.create (Vector2 180 20) "Choice" ["Foo", "Bar", "Baz"] 0
    UICmd.register parent widget def


    let values = (AnyLunaValue <$> ([1, 2, 3, 4, 5, 6, 7] :: [Int]))
              <> (AnyLunaValue <$> ([0.1, 0.2, 0.3] :: [Double]))
              <> (AnyLunaValue <$> [True, False, True])
        widget = List.createList "List" values $ AnyLunaValue (-1 :: Int)
    UICmd.register_ parent widget def

    Layout.verticalLayout 10.0 parent

    return ()

toAction _  = Nothing
