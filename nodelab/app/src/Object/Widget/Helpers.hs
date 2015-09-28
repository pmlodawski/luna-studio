module Object.Widget.Helpers where

import           Utils.PreludePlus
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry

import           Object.UITypes
import           Object.Node
import           Object.Object
import qualified Object.Widget.Node    as WNode
import qualified Object.Widget         as Widget
import           Object.Widget

nodeIdToWidgetId :: forall a. UIRegistry.State a -> NodeId -> Maybe WidgetId
nodeIdToWidgetId state nodeId = (view Widget.objectId) <$> matching where
    files   :: [WidgetFile a WNode.Node]
    files    = UIRegistry.lookupAll state
    matching = find (\file -> (file ^. Widget.widget . WNode.nodeId) == nodeId) files
