module Reactive.Plugins.Core.Action.Backend.AddNode where

import           Utils.PreludePlus
import           Event.Event        (Event(Batch))
import qualified Event.Batch        as Batch

import           Reactive.State.Global      (State)
import           Reactive.Commands.AddNode  (addNode)
import           Reactive.Commands.Graph    (localConnectNodes)
import           Reactive.Commands.Command  (Command)
import           Empire.API.Response (_Update)
import qualified Empire.API.Response as Response
import qualified Empire.API.Graph.AddNode as AddNode
import qualified Empire.API.Graph.Connect as Connect
import qualified Empire.API.Data.Node as Node
import           Empire.API.Data.PortRef (InPortRef(..), OutPortRef(..))
import qualified Empire.API.Data.PortRef as PortRef

toAction :: Event -> Maybe (Command State ())
toAction (Batch (Batch.NodeAdded      response)) = addNode           <$> (response ^? _Update . _2 . AddNode.node)
toAction (Batch (Batch.NodesConnected response)) = Just $ mapM_ processConnection (response ^? _Update . _1)  where
    processConnection request = localConnectNodes (request ^. Connect.src) (request ^. Connect.dst)
toAction _                              = Nothing
