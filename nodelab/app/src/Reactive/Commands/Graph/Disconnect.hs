module Reactive.Commands.Graph.Disconnect
     ( disconnectAll
     , localDisconnectAll
     ) where

import           Utils.PreludePlus

import           Reactive.Commands.Command    (Command)
import           Reactive.Commands.Graph      (connectionIdToWidgetId)
import           Reactive.Commands.UIRegistry (removeWidget)
import           Reactive.State.Global        (State)
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.Graph         as Graph

import qualified Reactive.Commands.Batch      as BatchCmd
import           Empire.API.Data.Connection   (Connection, ConnectionId)
import qualified Empire.API.Data.Connection   as Connection
import           Empire.API.Data.PortRef      (InPortRef, OutPortRef)

localDisconnectAll :: [ConnectionId] -> Command State ()
localDisconnectAll connectionIds = do
    uiRegistry             <- use Global.uiRegistry
    graph                  <- use Global.graph

    widgetIds <- mapM ((zoom Global.uiRegistry) . connectionIdToWidgetId) connectionIds

    zoom Global.uiRegistry $ mapM removeWidget $ catMaybes widgetIds
    Global.graph           %= Graph.removeConnections connectionIds

connectionToRefs :: Connection -> (OutPortRef, InPortRef)
connectionToRefs conn = (conn ^. Connection.src, conn ^. Connection.dst)

disconnectAll :: [ConnectionId] -> Command State ()
disconnectAll connectionIds = do
    graph     <- use Global.graph
    let conns = catMaybes $ Graph.lookUpConnection graph <$> connectionIds
        refs  = connectionToRefs <$> conns
    mapM_ BatchCmd.disconnectNodes (snd <$> refs)
    localDisconnectAll connectionIds
