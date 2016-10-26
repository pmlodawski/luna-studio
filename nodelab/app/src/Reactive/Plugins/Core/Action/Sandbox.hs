{-# LANGUAGE OverloadedStrings #-}
module Reactive.Plugins.Core.Action.Sandbox where

import           Utils.PreludePlus
import           Utils.Vector

import           Event.Event                     (Event (..))
import qualified Event.Keyboard                  as Keyboard
import           Reactive.Commands.Command       (Command, performIO)
import qualified Reactive.State.Global           as Global

import qualified Reactive.Commands.UIRegistry    as UICmd
import           Reactive.State.UIRegistry       (addHandler, sceneGraphId)

import qualified Object.Widget.CodeEditor        as CodeEditor
import qualified Object.Widget.DefinitionPort    as DefinitionPort
import qualified Object.Widget.Graphics          as G
import qualified Object.Widget.Group             as Group
import qualified Object.Widget.LabeledTextBox    as LabeledTextBox
import qualified Object.Widget.List              as List
import qualified Object.Widget.Number.Continuous as ContinuousNumber
import qualified Object.Widget.Number.Discrete   as DiscreteNumber
import qualified Object.Widget.Slider.Continuous as ContinuousSlider
import qualified Object.Widget.Slider.Discrete   as DiscreteSlider
import qualified Object.Widget.Toggle            as Toggle
import           UI.Handlers.DefinitionPort      ()

import qualified Reactive.Commands.Batch         as BatchCmd

import           Object.LunaValue
import           Object.LunaValue.Instances      ()
import           Reactive.State.Global           (inRegistry)
import           UI.Handlers.Generic             (ValueChangedHandler (..))
import           UI.Layout                       as Layout


f1, f2 :: Char
f1 = '\112'
f2 = '\113'

toAction :: Event -> Maybe (Command Global.State ())
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\112' _)) = Just $ Global.inRegistry $ do
    performIO $ putStrLn "show sandbox"
    let widget = Group.create
    parent <- UICmd.register sceneGraphId widget (Layout.verticalLayoutHandler 5.0)

    let widget = DefinitionPort.create (Vector2 100 100) False "x :: X" DefinitionPort.Input
    UICmd.register_ parent widget def

    let widget = DefinitionPort.create (Vector2 100 100) False "output" DefinitionPort.Output
    UICmd.register_ parent widget def

    let widget = DiscreteNumber.create (Vector2 200 20) "Discrete" 42
    UICmd.register_ parent widget def

    let widget = ContinuousNumber.create (Vector2 200 20) "Continuous" 42.42
    UICmd.register_ parent widget def

    let widget = ContinuousSlider.create (Vector2 200 20) "ContinuousSlider" (-2.0) 5.0 3.0
    UICmd.register_ parent widget def

    let widget = DiscreteSlider.create (Vector2 200 20) "DiscreteSlider" (-15) 80 30
    UICmd.register_ parent widget def

    let widget = DiscreteSlider.create (Vector2 200 20) "DiscreteSlider" 3 8 4
    UICmd.register_ parent widget def

    let widget = CodeEditor.create (Vector2 300 150) "class Point:\n    x y z :: Int\n  origin      = Point 0 0 0\n  Point x y _ = origin\n  print 'Origin XY coords are ($x,$y)'"
    UICmd.register_ parent widget def

    let widget = Toggle.create (Vector2 200 20) "Toggle" True
    UICmd.register_ parent widget def

    let widget = LabeledTextBox.create (Vector2 200 20) "Textbox" "foo"
    UICmd.register_ parent widget def

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
    UICmd.register_ flexParent widget def
    UICmd.register_ flexParent widget def
    UICmd.register_ flexParent widget def
    UICmd.register_ flexParent widget def
    UICmd.register_ flexParent widget def
    UICmd.register_ flexParent widget def
    -- UICmd.moveX resizedParent 200

    let widget = ContinuousNumber.create (Vector2 200 20) "Width" 200
    UICmd.register_ parent widget $ addHandler (ValueChangedHandler $ \val _ -> inRegistry $ do
        UICmd.resize' resizableWidget  (x .~ val)
        UICmd.resize' resizableWidget3 (x .~ val)
        ) mempty

    let widget = ContinuousNumber.create (Vector2 200 20) "Height" 20
    UICmd.register_ parent widget $ addHandler (ValueChangedHandler $ \val _ -> inRegistry $ do
        UICmd.resize' resizableWidget  (y .~ val)
        UICmd.resize' resizableWidget3 (y .~ val)
        ) mempty

    let widget = ContinuousNumber.create (Vector2 200 20) "FlexHeight" 100
    UICmd.register_ parent widget $ addHandler (ValueChangedHandler $ \val _ -> inRegistry $ do
        UICmd.resize' flexParent (y .~ val)
        ) mempty

    return ()
toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\115' _)) = Just $ Global.inRegistry $ do
    performIO $ putStrLn "show sandbox"
    performIO $ putStrLn "show sandbox"
    let widget = Group.create
    parent <- UICmd.register sceneGraphId widget (Layout.verticalLayoutHandler 5.0)

    let shader  = "void main() { gl_FragColor = vec4(0.1, 0.0, 0.0, 1.0); } "
        shader2 = "void main() { gl_FragColor = vec4(1.0, 1.0, 0.0, 1.0); } "
        n       = 4.0
        n'      = 4
        items   = [ G.Item shader  [ G.Box (Vector2 0.0 0.0) ] (Vector2 1.0 1.0) (Vector2 0.0 0.0)
                  , G.Item shader2 [ G.Box (Vector2 (i / n) 0.0) | i <- [1..n']] (Vector2 1.0 1.0) (Vector2 0.0 0.0)
                  ]
        labels  = []
        widget = G.create (Vector2 400 400) items labels
    UICmd.register_ parent widget def

    return ()

toAction (Keyboard _ (Keyboard.Event Keyboard.Down '\116' _)) = Just BatchCmd.dumpGraphViz

toAction _  = Nothing
