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
import           Utils.Vector

import           Object.LunaValue

import qualified Object.Widget.List               as List
import qualified Object.Widget.Number.Discrete    as DiscreteNumber
import qualified UI.Handlers.Number.Discrete      as DiscreteNumber
import           UI.Handlers.Generic              (triggerValueChanged, ValueChangedHandler(..))
import qualified Object.Widget.Number.Continuous  as ContinuousNumber
import qualified UI.Handlers.Number.Continuous    as ContinuousNumber
import qualified UI.Handlers.Toggle               as Toggle
import qualified Object.Widget.Toggle             as Toggle


controlHandler :: LunaValue a => a -> WidgetId -> Command Global.State ()
controlHandler val id = triggerValueChanged (AnyLunaValue val) id

instance LunaValue Int    where
    asLunaExpr = LunaExpression . show
    createValueWidget' parent val label width handlers = do
        let widget    = DiscreteNumber.create (Vector2 width 20) label val
            handlers' = addHandler (ValueChangedHandler $ (controlHandler :: Int -> WidgetId -> Command Global.State () )) $ handlers
        UICmd.register parent widget handlers'

instance LunaValue Double where
    asLunaExpr = LunaExpression . show
    createValueWidget' parent val label width handlers = do
        let widget    = ContinuousNumber.create (Vector2 width 20) label val
            handlers' = addHandler (ValueChangedHandler $ (controlHandler :: Double -> WidgetId -> Command Global.State () )) $ handlers
        UICmd.register parent widget handlers'

instance LunaValue Bool   where
    asLunaExpr = LunaExpression . show
    createValueWidget' parent val label width handlers = do
        let widget    = Toggle.create (Vector2 width 20) label val
            handlers' = addHandler (ValueChangedHandler $ (controlHandler :: Bool -> WidgetId -> Command Global.State () )) $ handlers
        UICmd.register parent widget handlers'
