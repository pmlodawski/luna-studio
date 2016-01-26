{-# LANGUAGE OverloadedStrings #-}
module UI.Handlers.Node where

import           Utils.PreludePlus

import           Data.HMap.Lazy                  (HTMap, TypeKey (..))
import qualified Data.HMap.Lazy                  as HMap
import qualified Data.Text.Lazy                  as Text
import           Data.Text.Lazy.Read             (rational)
import           Utils.Vector

import           Event.Event                     (JSState)
import           Object.Widget                   (DblClickHandler, DragEndHandler, DragMoveHandler, KeyUpHandler,
                                                  MousePressedHandler, UIHandlers, WidgetId, currentPos, dblClick,
                                                  dragEnd, dragMove, keyMods, keyUp, mousePressed, startPos,
                                                  CompositeWidget, createWidget, updateWidget, ResizableWidget, resizeWidget)
import qualified Object.Widget.Node              as Model
import qualified Object.Widget.Group             as Group
import qualified Object.Widget.Label             as Label
import           Reactive.Commands.Command       (Command, performIO)
import qualified Reactive.Commands.UIRegistry    as UICmd
import           Reactive.State.Global           (inRegistry)
import qualified Reactive.State.Global           as Global
import           Reactive.State.UIRegistry       (addHandler)
import qualified Reactive.State.UIRegistry       as UIRegistry

import           UI.Generic                      (startDrag, takeFocus, defaultResize)
import           UI.Handlers.Generic             (triggerValueChanged)
import           UI.Layout                       as Layout
import           UI.Widget.Group ()
import           UI.Widget.Label ()
import           UI.Handlers.Generic             (ValueChangedHandler (..), triggerValueChanged)

instance CompositeWidget Model.Node where
    createWidget id model = do
        let offset = Vector2 (-30.0) 70.0
            grp    = Group.Group offset def True Nothing
        UICmd.register id grp (Layout.verticalLayoutHandler def 5.0)
        let offset = Vector2 0 40.0
            label  = Label.Label offset (Vector2 100.0 20.0) "?"
        void $ UICmd.register id label def

    updateWidget id old model = do
        (controlsId:labelId:_) <- UICmd.children id
        UICmd.update_ controlsId $ Group.visible .~ (model ^. Model.isExpanded)
        UICmd.update_ labelId    $ Label.label   .~ (model ^. Model.value)

