module Reactive.Plugins.Core.Action.State.UIRegistry where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Widget

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import           Utils.CtxDynamic


type WidgetId  = Int
type WidgetMap = IntMap DisplayObject

data State = State { _widgets  :: WidgetMap
                   , _nextId   :: WidgetId
                   }

makeLenses ''State

instance Eq State where
    a == b = (a ^. nextId) == (b ^. nextId)
instance Show State where
    show a = show $ a ^. nextId



instance Default State where
    def = State def def

instance PrettyPrinter State where
    display (State widgets nid) = "dWd(" <> show nid <> " / " <> (show $ IntMap.size widgets) <> ")"


register :: DisplayObjectClass a => WidgetMap -> a -> WidgetMap
register m a = IntMap.insert (objectId a) (toCtxDynamic a) m

registerAll :: DisplayObjectClass a => WidgetMap -> [a] -> WidgetMap
registerAll = foldl register

unregister :: DisplayObjectClass a => WidgetMap -> a -> WidgetMap
unregister m a = IntMap.delete (objectId a) m

unregisterAll :: DisplayObjectClass a => WidgetMap -> [a] -> WidgetMap
unregisterAll = foldl unregister

replaceAll :: DisplayObjectClass a => WidgetMap -> [a] -> [a] -> WidgetMap
replaceAll m r = registerAll $ unregisterAll m r