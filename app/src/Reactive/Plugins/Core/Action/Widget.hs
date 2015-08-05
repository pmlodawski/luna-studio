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
import           Reactive.Plugins.Core.Action.State.UIRegistry   ( WidgetMap, WidgetId )
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
toAction (Mouse m@(Mouse.Event _ _ _ _ _ _)) = Just $ MouseAction m
toAction _                                   = Nothing

handleOther :: Mouse.Event -> Maybe WidgetId -> WidgetMap -> Maybe (WidgetUIUpdate, WidgetMap)
handleOther mouseEvent Nothing m = Nothing
handleOther mouseEvent mWidget m = do
    widgetId <- mWidget
    widget   <- IntMap.lookup widgetId m
    (uiUpdate, newWidget) <- return $ case mouseEvent of
        Mouse.Event Mouse.Moved      _ _      _ (Just _) (Just pos) -> onMouseMove           pos widget
        Mouse.Event Mouse.Pressed    _ button _ (Just _) (Just pos) -> onMousePress   button pos widget
        Mouse.Event Mouse.Released   _ button _ (Just _) (Just pos) -> onMouseRelease button pos widget
        Mouse.Event Mouse.Clicked    _ _      _ (Just _) (Just pos) -> onClick               pos widget
        Mouse.Event Mouse.DblClicked _ _      _ (Just _) (Just pos) -> onDblClick            pos widget
        _                                                           -> (Nothing, widget)
    let newM = IntMap.insert (objectId widget) newWidget m
    return (uiUpdate, newM) where

instance ActionStateUpdater Action where
    execSt (MouseAction mouseEvent) oldState = case newAction of
            Just action -> ActionUI newAction newState
            Nothing     -> ActionUI  NoAction newState
        where
        newAction               = Just $ ApplyUpdates uiUpdates
        newState                = oldState &  Global.uiRegistry . UIRegistry.widgets    .~ newWidgets
                                           &  Global.uiRegistry . UIRegistry.widgetOver .~ widgetOver
        oldWidgetOver           = oldState ^. Global.uiRegistry . UIRegistry.widgetOver
        oldWidgets              = oldState ^. Global.uiRegistry . UIRegistry.widgets
        (uiUpdates, newWidgets) = UIRegistry.sequenceUpdates [ handleMouseOut
                                                             , handleMouseOver
                                                             , Just $ (handleOther mouseEvent widgetOver)
                                                             ] oldWidgets
        (handleMouseOver, handleMouseOut, widgetOver) = case mouseEvent of
            Mouse.Event Mouse.Moved _ _ _ bid _ -> (handleMouseOver', handleMouseOut', widgetOver) where
                widgetOver = bid
                widgetOverChanged = oldWidgetOver /= widgetOver
                handleMouseOver' = case widgetOverChanged of
                    True  -> Just $ \m -> do
                        oid <- oldWidgetOver
                        oldOut <- IntMap.lookup oid m
                        (a, w) <- Just $ onMouseOut oldOut
                        Just (a, IntMap.insert oid w m)

                    False -> Nothing
                handleMouseOut' = case widgetOverChanged of
                    True  -> Just $ \m -> do
                        oid <- widgetOver
                        oldOver <- IntMap.lookup oid m
                        (a, w) <- Just $ onMouseOver oldOver
                        Just (a, IntMap.insert oid w m)
                    False -> Nothing
            _               -> (Nothing, Nothing, oldWidgetOver)

instance ActionUIUpdater Action where
    updateUI (WithState (ApplyUpdates actions) state) = sequence_ $ catMaybes actions
