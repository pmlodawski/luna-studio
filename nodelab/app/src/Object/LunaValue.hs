{-# LANGUAGE ExistentialQuantification #-}

module Object.LunaValue where

import           Utils.PreludePlus         hiding (Choice)
import qualified Reactive.State.UIRegistry as UIRegistry
import           Object.UITypes            (WidgetId)
import           Reactive.Commands.Command (Command)
import           Data.HMap.Lazy (HTMap)
import           Data.Aeson (ToJSON, toJSON)

newtype LunaExpression = LunaExpression String

instance Show LunaExpression where
    show (LunaExpression s) = s

class (Eq a, Show a, ToJSON a) => LunaValue a where
    asLunaExpr           :: a        -> LunaExpression
    createValueWidget'   :: WidgetId -> a   -> Text -> HTMap -> Command UIRegistry.State WidgetId
    listHandlers'        :: WidgetId -> WidgetId -> WidgetId -> a   -> HTMap
data AnyLunaValue = forall a. LunaValue a => AnyLunaValue {unAnyLunaValue :: a}

createValueWidget :: WidgetId -> AnyLunaValue -> Text -> HTMap -> Command UIRegistry.State WidgetId
createValueWidget id (AnyLunaValue a) label handlers = createValueWidget' id a label handlers

listHandlers :: WidgetId -> WidgetId -> WidgetId -> AnyLunaValue -> HTMap
listHandlers listId groupId rowId (AnyLunaValue a) = listHandlers' listId groupId rowId a

deriving instance Show AnyLunaValue
instance ToJSON AnyLunaValue where
    toJSON (AnyLunaValue a) = toJSON a
