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
import qualified Reactive.Commands.UIRegistry as UICmd

import           Object.Widget.Number.Discrete (DiscreteNumber(..))
import qualified UI.Handlers.Number.Discrete as DiscreteNumber
import           UI.Widget.Number.Discrete ()

import           Object.Widget.Number.Continuous  (ContinuousNumber(..))
import qualified UI.Handlers.Number.Continuous as ContinuousNumber
import           UI.Widget.Number.Continuous ()

import           Object.Widget.Slider.Discrete (DiscreteSlider(..))
import qualified UI.Handlers.Slider.Discrete as DiscreteSlider
import           UI.Widget.Slider.Discrete ()

import           Object.Widget.Slider.Continuous  (ContinuousSlider(..))
import qualified UI.Handlers.Slider.Continuous as ContinuousSlider
import           UI.Widget.Slider.Continuous ()

import           Object.Widget.Toggle  (Toggle(..))
import qualified UI.Handlers.Toggle as Toggle
import           UI.Widget.Toggle ()

import           Object.Widget.TextBox  (TextBox(..))
import qualified UI.Handlers.TextBox as TextBox
import           UI.Widget.TextBox ()

import           Object.Widget.Choice.RadioButton  (RadioButton(..))
import qualified UI.Handlers.Choice.RadioButton as RadioButton
import           UI.Widget.Choice.RadioButton ()

import           Object.Widget.Choice  (Choice(..))
import qualified Object.Widget.Choice  as Choice
import qualified UI.Handlers.Choice    as Choice
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
    let parent = sceneGraphId

    let widget = DiscreteNumber (Vector2 10 0) (Vector2 180 20) ("DiscreteNumber") 42 True Nothing
    UICmd.register_ parent widget def

    let widget = ContinuousNumber (Vector2 10 30) (Vector2 180 20) ("ContinuousNumber") 42.42 True Nothing
    UICmd.register_ parent widget def

    let widget = ContinuousSlider (Vector2 10 60) (Vector2 180 20) ("ContinuousSlider") True (-2.0) 5.0 3.0 Nothing
    UICmd.register_ parent widget def

    let widget = DiscreteSlider (Vector2 10 90) (Vector2 180 20) ("DiscreteSlider") True (-20) 80 30 Nothing
    UICmd.register_ parent widget def

    let widget = DiscreteSlider (Vector2 10 120) (Vector2 180 20) ("DiscreteSlider") True 3 8 4 Nothing
    UICmd.register_ parent widget def

    let widget = Toggle (Vector2 10 150) (Vector2 180 20) ("Toggle") True True True
    UICmd.register_ parent widget def

    let widget = TextBox (Vector2 10 180) (Vector2 180 20) ("TextBox") "foo" False
    UICmd.register_ parent widget def

    let widget = Choice (Vector2 10 210) (Vector2 180 20) "" ["Foo", "Bar", "Baz"] 0
    makeChoice parent widget

    return ()

toAction _  = Nothing



radioHandlers :: WidgetId -> Word -> HTMap
radioHandlers id ix = addHandler (RadioButton.SelectedHandler $ selectRadioButton id ix)
                    $ mempty

selectRadioButton :: WidgetId -> Word -> Command Global.State ()
selectRadioButton id ix = inRegistry $ UICmd.update_ id $ Choice.value .~ ix

makeChoice :: WidgetId -> Choice -> Command UIRegistry.State WidgetId
makeChoice parent model = do
    contId <- UICmd.register parent model def
    let opts = (model ^. Choice.options) `zip` [0..]
    forM_ opts $ \(label, ix) -> do
        let widget = RadioButton def (Vector2 180 20) label False
        UICmd.register_ contId widget (radioHandlers contId ix)

    Layout.verticalLayout 5.0 contId

    return contId

