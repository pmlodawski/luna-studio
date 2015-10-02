module Object.Widget.Helpers where

import           Utils.PreludePlus
import qualified Reactive.Plugins.Core.Action.State.UIRegistry as UIRegistry

import           Object.UITypes           (WidgetId)
import           Object.Object            (ConnectionId, NodeId)
import qualified Object.Widget.Node       as WNode
import qualified Object.Widget.Connection as WConnection
import qualified Object.Widget            as Widget
import           Object.Widget            (WidgetFile)

nodeIdToWidgetId :: forall a. UIRegistry.State a -> NodeId -> Maybe WidgetId
nodeIdToWidgetId state nodeId = view Widget.objectId <$> matching where
    files   :: [WidgetFile a WNode.Node]
    files    = UIRegistry.lookupAll state
    matching = find (\file -> (file ^. Widget.widget . WNode.nodeId) == nodeId) files

connectionIdToWidgetId :: forall a. UIRegistry.State a -> ConnectionId -> Maybe WidgetId
connectionIdToWidgetId state connectionId = view Widget.objectId <$> matching where
    files   :: [WidgetFile a WConnection.Connection]
    files    = UIRegistry.lookupAll state
    matching = find (\file -> (file ^. Widget.widget . WConnection.connectionId) == connectionId) files
