module Reactive.Plugins.Core.Action.State.UIRegistry where


import           Utils.PreludePlus
import           Utils.Vector

import           Object.Object
import           Object.Widget

import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap



type WidgetId  = Int
type WidgetMap = IntMap

data State = State { _widgets  :: WidgetMap DisplayObject
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

