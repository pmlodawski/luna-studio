{-# LANGUAGE ExistentialQuantification #-}

module Object.LunaValue where

import           Utils.PreludePlus         hiding (Choice)
import qualified Reactive.State.UIRegistry as UIRegistry
import qualified Reactive.State.Global as Global
import           Object.UITypes            (WidgetId)
import           Reactive.Commands.Command (Command)
import           Data.HMap.Lazy (HTMap)
import           Data.Aeson (ToJSON, toJSON)
import           UI.Handlers.Generic

newtype LunaExpression = LunaExpression String

instance Show LunaExpression where
    show (LunaExpression s) = s

class (Eq a, Show a, ToJSON a) => LunaValue a where
    asLunaExpr           :: a -> LunaExpression
    createValueWidget'   :: WidgetId -> a -> Text -> HTMap -> Command UIRegistry.State WidgetId

data AnyLunaValue = forall a. LunaValue a => AnyLunaValue {unAnyLunaValue :: a}

createValueWidget :: WidgetId -> AnyLunaValue -> Text -> HTMap -> Command UIRegistry.State WidgetId
createValueWidget id (AnyLunaValue a) label handlers = createValueWidget' id a label handlers

deriving instance Show AnyLunaValue
instance ToJSON AnyLunaValue where
    toJSON (AnyLunaValue a) = toJSON a
