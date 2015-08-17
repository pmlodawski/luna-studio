module Reactive.Plugins.Core.Action.State.UIRegistry where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Widget

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap

import           Event.Mouse (WidgetId, MouseButton)

import           Utils.CtxDynamic

type WidgetMap = IntMap DisplayObject

data State = State { _widgets         :: WidgetMap
                   , _widgetOver      :: Maybe WidgetId
                   , _dragState       :: Maybe DragState
                   }

makeLenses ''State

instance Eq State where
    a == b = (IntMap.size $ a ^. widgets) == (IntMap.size $ a ^. widgets)
instance Show State where
    show a = show $ IntMap.size $ a ^. widgets

instance Default State where
    def = State def def def

instance PrettyPrinter State where
    display (State widgets wover dragging) =
           "dWd("        <> show (IntMap.keys widgets)
        <> " over: "     <> show wover
        <> " dragging: " <> show dragging
        <> ")"

lookup :: WidgetId -> State -> Maybe DisplayObject
lookup idx state = IntMap.lookup idx oldWidgets where
        oldWidgets = state ^. widgets

register, unregister :: DisplayObjectClass a => a -> State -> State
register a state = state & widgets .~ newWidgets where
    newWidgets = IntMap.insert (objectId a) (toCtxDynamic a) oldWidgets
    oldWidgets = state ^. widgets

update :: DisplayObject -> State -> State
update a state = state & widgets .~ newWidgets where
    newWidgets = IntMap.insert (objectId a) a oldWidgets
    oldWidgets = state ^. widgets

unregister a state = state & widgets .~ newWidgets where
    newWidgets = IntMap.delete (objectId a) oldWidgets
    oldWidgets = state ^. widgets

registerAll, unregisterAll :: DisplayObjectClass a => [a] -> State -> State
registerAll   a state = foldl (flip   register) state a
unregisterAll a state = foldl (flip unregister) state a

generateIds :: Int -> State -> [WidgetId]
generateIds count state = [startId..startId + count] where
    startId = if IntMap.size (state ^. widgets) == 0 then 1
                                                     else maxId + 1 where (maxId, _) = IntMap.findMax (state ^. widgets)

replaceAll :: DisplayObjectClass a => [a] -> [a] -> State -> State
replaceAll remove add state = registerAll add $ unregisterAll remove state

sequenceUpdates :: [Maybe (State -> Maybe (WidgetUIUpdate, State))]
                -> State -> ([WidgetUIUpdate], State)
sequenceUpdates ops input = foldl applyOp ([], input) ops where
    applyOp (updates, input) op = fromMaybe (updates, input) $ do
        justOp           <- op
        (update, output) <- justOp input
        return (update:updates, output)
