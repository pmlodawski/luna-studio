module Reactive.Commands.Graph.Disconnect
     ( disconnectAll
     , localDisconnectAll
     ) where

import           Utils.PreludePlus

import qualified Data.IntMap.Lazy             as IntMap
import qualified Data.Map.Lazy                as Map

import           Reactive.Commands.Command    (Command, performIO)
import           Reactive.Commands.Graph      (connectionIdToWidgetId, updateConnNodes, updateConnections)
import           Reactive.Commands.UIRegistry (removeWidget)
import           Reactive.State.Global        (State)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.Graph         as Graph
import qualified Reactive.State.UIRegistry    as UIRegistry

import qualified Reactive.Commands.Batch      as BatchCmd
import           Control.Monad.State          hiding (State)
import           Empire.API.Data.Connection   (Connection, ConnectionId)
import qualified Empire.API.Data.Connection   as Connection
import           Empire.API.Data.Node         (NodeId)
import           Empire.API.Data.PortRef      (InPortRef, OutPortRef)

getChangedNodes :: Graph.State -> [ConnectionId] -> [NodeId]
getChangedNodes graph connIds = nIds1 ++ nIds2
    where (nIds1, nIds2) = connIds & map (flip Graph.getConnectionNodeIds graph)
                                   & catMaybes
                                   & unzip


localDisconnectAll :: [ConnectionId] -> Command State ()
localDisconnectAll connectionIds = do
    uiRegistry             <- use Global.uiRegistry
    graph                  <- use Global.graph

    widgetIds <- mapM ((zoom Global.uiRegistry) . connectionIdToWidgetId) connectionIds

    let changedNodes       = getChangedNodes graph connectionIds

    zoom Global.uiRegistry $ mapM removeWidget $ catMaybes widgetIds
    Global.graph           %= Graph.removeConnections connectionIds
    updateConnNodes changedNodes
    updateConnections

connectionToRefs :: Connection -> (OutPortRef, InPortRef)
connectionToRefs conn = (conn ^. Connection.src, conn ^. Connection.dst)

disconnectAll :: [ConnectionId] -> Command State ()
disconnectAll connectionIds = do
    graph     <- use Global.graph
    let conns = catMaybes $ Graph.lookUpConnection graph <$> connectionIds
        refs  = connectionToRefs <$> conns
    mapM_ BatchCmd.disconnectNodes (snd <$> refs)
    localDisconnectAll connectionIds
