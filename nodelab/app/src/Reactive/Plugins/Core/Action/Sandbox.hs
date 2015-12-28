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
import           UI.Handlers.Generic              (ValueChangedHandler(..))


f1 = '\112'
f2 = '\113'

toAction :: Event -> Maybe (Command Global.State ())
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\112' _)) = Just $ Global.inRegistry $ do
    performIO $ putStrLn "show sandbox"
    let widget = Group.create
    parent <- UICmd.register sceneGraphId widget (Layout.verticalLayoutHandler 5.0)

    let widget = DiscreteNumber.create (Vector2 200 20) "Discrete" 42
    UICmd.register parent widget def

    let widget = ContinuousNumber.create (Vector2 200 20) "Continuous" 42.42
    UICmd.register parent widget def

    let widget = ContinuousSlider.create (Vector2 200 20) "ContinuousSlider" (-2.0) 5.0 3.0
    UICmd.register parent widget def

    let widget = DiscreteSlider.create (Vector2 200 20) "DiscreteSlider" (-15) 80 30
    UICmd.register parent widget def

    let widget = DiscreteSlider.create (Vector2 200 20) "DiscreteSlider" 3 8 4
    UICmd.register parent widget def

    let widget = Toggle.create (Vector2 200 20) "Toggle" True
    UICmd.register_ parent widget def

    let widget = LabeledTextBox.create (Vector2 200 20) "Textbox" "foo"
    UICmd.register_ parent widget def

    let widget = Choice.create (Vector2 200 20) "Choice" ["Foo", "Bar", "Baz"] 0
    UICmd.register parent widget def


    let values = (AnyLunaValue <$> ([1, 2, 3, 4, 5, 6, 7] :: [Int]))
              <> (AnyLunaValue <$> ([0.1, 0.2, 0.3] :: [Double]))
              <> (AnyLunaValue <$> [True, False, True])
        widget = List.createList 200 "List" values $ AnyLunaValue (-1 :: Int)
    UICmd.register_ parent widget def

    return ()

toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\113' _)) = Just $ Global.inRegistry $ do
    performIO $ putStrLn "show sandbox"
    let widget = Group.create
    parent'       <- UICmd.register sceneGraphId widget (Layout.horizontalLayoutHandler 5.0)
    parent        <- UICmd.register parent'      widget (Layout.verticalLayoutHandler 5.0)
    resizedParent <- UICmd.register parent'      widget (Layout.verticalLayoutHandler 5.0)
    flexParent    <- UICmd.register parent'      widget (Layout.flexVerticalLayoutHandler 5.0)

    let widget = ContinuousSlider.create (Vector2 200 20) "ContinuousSlider" (-2.0) 5.0 3.0
    resizableWidget  <- UICmd.register resizedParent widget def
    resizableWidget2 <- UICmd.register resizedParent widget def
    resizableWidget3 <- UICmd.register resizedParent widget def
    UICmd.register flexParent widget def
    UICmd.register flexParent widget def
    UICmd.register flexParent widget def
    UICmd.register flexParent widget def
    UICmd.register flexParent widget def
    UICmd.register flexParent widget def
    -- UICmd.moveX resizedParent 200

    let widget = ContinuousNumber.create (Vector2 200 20) "Width" 200
    UICmd.register parent widget $ addHandler (ValueChangedHandler $ \val _ -> inRegistry $ do
        UICmd.resize' resizableWidget  (x .~ val)
        UICmd.resize' resizableWidget3 (x .~ val)
        ) $ mempty

    let widget = ContinuousNumber.create (Vector2 200 20) "Height" 20
    UICmd.register parent widget $ addHandler (ValueChangedHandler $ \val _ -> inRegistry $ do
        UICmd.resize' resizableWidget  (y .~ val)
        UICmd.resize' resizableWidget3 (y .~ val)
        ) $ mempty

    let widget = ContinuousNumber.create (Vector2 200 20) "FlexHeight" 100
    UICmd.register parent widget $ addHandler (ValueChangedHandler $ \val _ -> inRegistry $ do
        UICmd.resize' flexParent (y .~ val)
        ) $ mempty

    return ()

toAction _  = Nothing
