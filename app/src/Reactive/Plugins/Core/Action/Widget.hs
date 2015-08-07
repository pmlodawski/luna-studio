{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.Widget where

import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Widget
import           Object.Node
import           Event.Mouse    hiding      (Event, WithObjects)
import qualified Event.Mouse    as Mouse
import qualified Event.Window   as Window
import           Event.Event
import           Event.WithObjects
import           Reactive.Plugins.Core.Action.Action
import qualified Reactive.Plugins.Core.Action.State.Global       as Global
import           Reactive.Plugins.Core.Action.State.UIRegistry   (WidgetMap)
import qualified Reactive.Plugins.Core.Action.State.UIRegistry   as UIRegistry
import qualified Object.Widget.Button as Button
import qualified Object.Widget.Slider as Slider
import           ThreeJS.Button ()
import           GHCJS.Prim
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import           Utils.CtxDynamic
import           Data.Set (Set)
import qualified Data.Set as Set
import           Debug.Trace

data Action = MouseAction   { _event   :: Mouse.Event }
            | ApplyUpdates  { _actions :: [WidgetUIUpdate] }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "WidgetAction"

toAction :: Event Node -> Maybe Action
toAction (Mouse m) = Just $ MouseAction m
toAction _         = Nothing

handleOther :: Mouse.Event -> UIRegistry.State -> Maybe (WidgetUIUpdate, UIRegistry.State)
handleOther (Mouse.Event eventType _ button _ (Just (EventWidget widgetId pos))) registry = do
    widget                <- (UIRegistry.lookup widgetId registry) :: Maybe DisplayObject
    (uiUpdate, newWidget) <- return $ case eventType of
        Mouse.Moved       -> onMouseMove    button pos widget
        Mouse.Pressed     -> onMousePress   button pos widget
        Mouse.Released    -> onMouseRelease button pos widget
        Mouse.Clicked     -> onClick               pos widget
        Mouse.DblClicked  -> onDblClick            pos widget
    let newRegistry = UIRegistry.update newWidget registry
    return (uiUpdate, registry)
handleOther _  _        = Nothing

instance ActionStateUpdater Action where
    execSt (MouseAction mouseEvent) oldState = case newAction of
            Just action -> ActionUI newAction newState
            Nothing     -> ActionUI  NoAction newState
        where
        newAction                = Just $ ApplyUpdates uiUpdates
        newState                 = oldState &  Global.uiRegistry .~ (newRegistry & UIRegistry.widgetOver .~ widgetOver)
        oldWidgetOver            = oldState ^. Global.uiRegistry . UIRegistry.widgetOver
        oldRegistry              = oldState ^. Global.uiRegistry
        (uiUpdates, newRegistry) = UIRegistry.sequenceUpdates [ handleMouseOut
                                                              , handleMouseOver
                                                              , Just $ (handleOther mouseEvent)
                                                              ] oldRegistry
        (handleMouseOver, handleMouseOut, widgetOver) = case mouseEvent of
            Mouse.Event Mouse.Moved _ _ _ evWd -> (handleMouseOver', handleMouseOut', widgetOver) where
                widgetOver = maybe Nothing (\x -> Just $ x ^. Mouse.widgetId) evWd
                widgetOverChanged = oldWidgetOver /= widgetOver
                handleMouseOut' = case widgetOverChanged of
                    True  -> Just $ \m -> do
                        oid <- oldWidgetOver
                        oldOut <- UIRegistry.lookup oid m
                        (a, w) <- Just $ onMouseOut oldOut
                        Just (a, UIRegistry.update w m)

                    False -> Nothing
                handleMouseOver' = case widgetOverChanged of
                    True  -> Just $ \m -> do
                        oid <- widgetOver
                        oldOver <- UIRegistry.lookup oid m
                        (a, w) <- Just $ onMouseOver oldOver
                        Just (a, UIRegistry.update w m)
                    False -> Nothing
            _               -> (Nothing, Nothing, oldWidgetOver)

instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates actions) state) = do
        putStrLn $ "Actions" <> show (length $ catMaybes actions)
        sequence_ $ catMaybes actions
