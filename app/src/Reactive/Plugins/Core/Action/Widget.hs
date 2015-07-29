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

data Action = MouseMoving   { _pos   :: Vector2 Int }
            | MousePressed  { _pos   :: Vector2 Int }
            | MouseReleased { _pos   :: Vector2 Int }
            | ApplyUpdates  { _actions :: [WidgetUIUpdate] }

makeLenses ''Action

instance PrettyPrinter Action where
    display _ = "WidgetAction"

toAction :: Event Node -> Maybe Action
toAction (Mouse (Mouse.Event tpe pos button _)) = case tpe of
    Mouse.Moved     -> Just $ MouseMoving pos
    _ -> Nothing
--     Mouse.Pressed   -> case button of
--         1 -> Just $ MousePressed pos
--         _ -> Nothing
--     Mouse.Released   -> case button of
--         1 -> Just $ MouseReleased pos
--         _ -> Nothing
toAction _           = Nothing

instance ActionStateUpdater Action where
    execSt newActionCandidate oldState = case newAction of
            Just action -> ActionUI newAction newState
            Nothing     -> ActionUI  NoAction newState
        where
        newAction = Just $ ApplyUpdates uiUpdates
        newState  = oldState & Global.uiRegistry . UIRegistry.widgets .~ newWidgets
        (uiUpdates, newWidgets) = IntMap.mapAccum processWidget [] oldWidgets
        processWidget actions widget = case newActionCandidate of
            MouseMoving pos -> (act:actions, newWidget)
                            where (act, newWidget) = onMouseMove pos widget
            _               -> (actions, widget)
        oldWidgets = oldState ^. Global.uiRegistry . UIRegistry.widgets

instance ActionUIUpdater Action where
    updateUI (WithState action state) = case action of
        ApplyUpdates actions -> do
            putStrLn $ "len " <> (show $ length actions)
            putStrLn $ show $ IntMap.size $ state ^. Global.uiRegistry . UIRegistry.widgets
            sequence_ actions
