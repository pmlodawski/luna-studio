{-# LANGUAGE OverloadedStrings #-}
module Object.LunaValue.Instances where

import           Utils.PreludePlus         hiding (Choice)
import qualified Reactive.State.UIRegistry as UIRegistry
import           Reactive.State.UIRegistry (addHandler)
import qualified Reactive.State.Global as Global
import           Reactive.State.Global (inRegistry)
import           Object.UITypes            (WidgetId)
import           Reactive.Commands.Command (Command)
import           Reactive.Commands.UIRegistry as UICmd
import           Data.HMap.Lazy (HTMap)
import           UI.Instances
import           Utils.Vector

import           Object.LunaValue


import qualified Object.Widget.List              as List
import qualified Object.Widget.Number.Discrete   as DiscreteNumber
import qualified UI.Handlers.Number.Discrete     as DiscreteNumber
import qualified UI.Handlers.List                as List
import qualified Object.Widget.Number.Continuous as ContinuousNumber
import qualified UI.Handlers.Number.Continuous   as ContinuousNumber
import qualified UI.Handlers.Toggle              as Toggle
import qualified Object.Widget.Toggle            as Toggle

controlHandler :: LunaValue a => WidgetId -> WidgetId -> WidgetId -> a -> WidgetId -> Command Global.State ()
controlHandler listWidget groupId rowId val id = do
    inRegistry $ do
        items <- UICmd.children groupId
        let idx = elemIndex rowId items
        forM_ idx $ \idx -> UICmd.update_ listWidget $ List.value . ix idx .~ (AnyLunaValue val)
    List.triggerValueChanged listWidget

instance LunaValue Int    where
    asLunaExpr = LunaExpression . show
    createValueWidget' parent val label handlers = do
        let widget = DiscreteNumber.create (Vector2 160 20) label val
        UICmd.register parent widget handlers
    listHandlers' listWidget groupWidget rowWidget _ = addHandler (DiscreteNumber.ValueChangedHandler $ controlHandler listWidget groupWidget rowWidget) $ mempty


instance LunaValue Double where
    asLunaExpr = LunaExpression . show
    createValueWidget' parent val label handlers = do
        let widget = ContinuousNumber.create (Vector2 160 20) label val
        UICmd.register parent widget handlers
    listHandlers' listWidget groupWidget rowWidget _ = addHandler (ContinuousNumber.ValueChangedHandler $ controlHandler listWidget groupWidget rowWidget) $ mempty

instance LunaValue Bool   where
    asLunaExpr = LunaExpression . show
    createValueWidget' parent val label handlers = do
        let widget = Toggle.create (Vector2 160 20) label val
        UICmd.register parent widget handlers
    listHandlers' listWidget groupWidget rowWidget _ = addHandler (Toggle.ValueChangedHandler $ controlHandler listWidget groupWidget rowWidget) $ mempty


