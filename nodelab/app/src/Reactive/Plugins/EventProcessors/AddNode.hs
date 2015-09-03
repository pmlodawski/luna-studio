module Reactive.Plugins.EventProcessors.AddNode where

import           Utils.PreludePlus
import           Reactive.Banana
import           Reactive.Banana.Frameworks
import qualified Event.Event                as Event
import qualified Event.Backend              as Backend
import qualified Event.AddNode              as AddNode
import           BatchConnector.Connection  (WebMessage(..))
import           BatchConnector.Updates     (parseAddNodeResponse, parseAddNodeFakeResponse)
import           Data.Dynamic
import           Data.ByteString.Lazy       (ByteString)
import           Object.Node

getAddNode :: forall t. Frameworks t => Event t (Event.Event Dynamic) -> Event t (Event.Event Dynamic)
getAddNode events = filterJust $ toAddNode <$> events

toAddNode :: Event.Event Dynamic -> Maybe (Event.Event Dynamic)
toAddNode (Event.Backend (Backend.Message msg)) = getNodeFromMessage msg
toAddNode _                                     = Nothing

getNodeFromMessage :: WebMessage -> Maybe (Event.Event Dynamic)
getNodeFromMessage (WebMessage "project.library.ast.function.graph.node.add.update" bytes) = (Event.AddNode . AddNode.AddNode) <$> parseAddNodeResponse bytes
getNodeFromMessage (WebMessage "project.library.ast.function.graph.node.add.fakeres" bytes) = (Event.AddNode . AddNode.AddNode) <$> parseAddNodeFakeResponse bytes
getNodeFromMessage _ = Nothing
