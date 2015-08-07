module Reactive.Plugins.Core.Action.State.UIRegistry where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Widget hiding (objectId)
import           Object.Widget.Types  (objectId)

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import           Event.Mouse (WidgetId)

import           Data.Set (Set)
import qualified Data.Set as Set

import           Utils.CtxDynamic

type WidgetMap = IntMap DisplayObject

data State = State { _widgets     :: WidgetMap
                   , _nextId      :: WidgetId
                   , _widgetOver  :: Maybe WidgetId
                   }

makeLenses ''State

instance Eq State where
    a == b = (a ^. nextId) == (b ^. nextId)
instance Show State where
    show a = show $ a ^. nextId



instance Default State where
    def = State def 1 def

instance PrettyPrinter State where
    display (State widgets nid wover) =
           "dWd("    <> show nid
        <> " / "     <> show (IntMap.size widgets)
        <> " over: " <> show wover
        <> ")"


register, unregister :: DisplayObjectClass a => WidgetMap -> a -> WidgetMap
register   m a = IntMap.insert (objectId a) (toCtxDynamic a) m
unregister m a = IntMap.delete (objectId a) m

registerAll, unregisterAll :: DisplayObjectClass a => WidgetMap -> [a] -> WidgetMap
registerAll   = foldl register
unregisterAll = foldl unregister

replaceAll :: DisplayObjectClass a => WidgetMap -> [a] -> [a] -> WidgetMap
replaceAll m r = registerAll $ unregisterAll m r

sequenceUpdates :: [Maybe (WidgetMap -> Maybe (WidgetUIUpdate, WidgetMap))]
                -> WidgetMap -> ([WidgetUIUpdate], WidgetMap)
sequenceUpdates ops input = foldl applyOp ([], input) ops where
    applyOp (updates, input) op = fromMaybe (updates, input) $ do
        justOp           <- op
        (update, output) <- justOp input
        return (update:updates, output)
