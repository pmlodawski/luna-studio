{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Widget where

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Widget
import           Object.Node
import           Event.Mouse    hiding      ( Event, WithObjects )
import qualified Event.Mouse    as Mouse
import qualified Event.Window   as Window
import           Event.Event
import           Event.WithObjects
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global       as Global
import qualified Reactive.Plugins.Core.Action.State.UIRegistry   as UIRegistry
import qualified Object.Widget.Button as Button
import           GHCJS.Prim
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import           Utils.CtxDynamic
import           Data.Set (Set)
import qualified Data.Set as Set

data Action = MouseAction   { _event :: Mouse.Event }
            | ApplyUpdates  { _actions :: [WidgetUIUpdate] }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "WidgetAction"

toAction :: Event Node -> Maybe Action
toAction (Mouse m@(Mouse.Event tpe pos button _)) = Just $ MouseAction m
toAction _                                        = Nothing


onlyIfOver :: MousePosition -> DisplayObject -> (MousePosition -> DisplayObject -> WidgetUpdate) -> WidgetUpdate
onlyIfOver pos widget f = if isOver pos widget then f pos widget else (Nothing, widget)

handleEvents :: Mouse.Event -> [WidgetUIUpdate] -> DisplayObject -> ([WidgetUIUpdate], DisplayObject)
handleEvents mouseEvent uiUpdates widget = (uiUpdate:uiUpdates, newWidget) where
    (uiUpdate, newWidget) = case mouseEvent of
        Mouse.Event Mouse.Moved      pos _      _ -> onlyIfOver pos widget $ onMouseMove
        Mouse.Event Mouse.Pressed    pos button _ -> onlyIfOver pos widget $ onMousePress   button
        Mouse.Event Mouse.Released   pos button _ -> onlyIfOver pos widget $ onMouseRelease button
        Mouse.Event Mouse.Clicked    pos _      _ -> onlyIfOver pos widget $ onClick
        Mouse.Event Mouse.DblClicked pos _      _ -> onlyIfOver pos widget $ onDblClick

instance ActionStateUpdater Action where
    execSt (MouseAction mouseEvent) oldState = case newAction of
            Just action -> ActionUI newAction newState
            Nothing     -> ActionUI  NoAction newState
        where
        newAction               = Just $ ApplyUpdates uiUpdates
        newState                = oldState & Global.uiRegistry . UIRegistry.widgets .~ newWidgets
                                           & Global.uiRegistry . UIRegistry.widgetOver .~ widgetOver
        (uiUpdates', newWidgets') = IntMap.mapAccum (handleEvents mouseEvent) [] oldWidgets
        oldWidgetOver          = oldState ^. Global.uiRegistry . UIRegistry.widgetOver
        widgetOver :: Maybe UIRegistry.WidgetId
        uiUpdates :: [WidgetUIUpdate]
        newWidgets :: UIRegistry.WidgetMap
        (uiUpdates, newWidgets, widgetOver) = case mouseEvent of
            Mouse.Event Mouse.Moved pos _ _ -> (uiOutOverUpdates, newOutOverWidgets, widgetOver) where
                maybeOver :: Maybe DisplayObject
                maybeOver = find (isOver pos) (IntMap.elems newWidgets')
                widgetOver :: Maybe UIRegistry.WidgetId
                widgetOver =  objectId <$> maybeOver
                (uiOutOverUpdates, newOutOverWidgets)      = if oldWidgetOver /= widgetOver then (ooUpdates, ooWidgets)
                                                                                              else (uiUpdates', newWidgets') where
                                                                                                   (newUIUpdates, newOutWidgets) = maybe (uiUpdates', newWidgets') id $ do
                                                                                                       oid <- oldWidgetOver
                                                                                                       oldOut <- IntMap.lookup oid newWidgets'
                                                                                                       (a, w) <- Just $ onMouseOut pos oldOut
                                                                                                       Just (a:uiUpdates', IntMap.insert oid w newWidgets')
                                                                                                   (ooUpdates, ooWidgets) = maybe (newUIUpdates, newOutWidgets) id $ do
                                                                                                       oid <- widgetOver
                                                                                                       oldOver <- IntMap.lookup oid newOutWidgets
                                                                                                       (a, w) <- Just $ onMouseOver pos oldOver
                                                                                                       Just (a:newUIUpdates, IntMap.insert oid w newOutWidgets)

            _               -> (uiUpdates', newWidgets', oldWidgetOver)


        oldWidgets              = oldState ^. Global.uiRegistry . UIRegistry.widgets



instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates actions) state) = sequence_ $ catMaybes actions
