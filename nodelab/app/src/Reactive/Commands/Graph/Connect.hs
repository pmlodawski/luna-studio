module Reactive.Commands.Graph.Connect
    ( batchConnectNodes
    , localConnectNodes
    ) where


import           Utils.PreludePlus

import qualified Object.Widget.Connection     as ConnectionModel
import qualified Reactive.Commands.Batch      as BatchCmd
import           Reactive.Commands.Command    (Command)
import qualified Reactive.Commands.UIRegistry as UICmd
import qualified Reactive.State.Global        as Global
import qualified Reactive.State.Graph         as Graph
import           Reactive.State.UIRegistry    (sceneGraphId)

import qualified Empire.API.Data.Port         as Port
import           Empire.API.Data.PortRef      (InPortRef (..), OutPortRef (..))
import           UI.Instances                 ()

withArrow :: Getter InPortRef Bool
withArrow = to $ \ref -> case ref of
    InPortRef _ Port.Self -> False
    _                     -> True

localConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ()
localConnectNodes src dst = do
    prevConn <- preuse $ Global.graph . Graph.connectionsMap . ix dst
    connectionId <- zoom Global.graph $ Graph.addConnection src dst
    let newConnection = not $ isJust prevConn
    when newConnection $ do
        widgetId <- zoom Global.uiRegistry $ UICmd.register sceneGraphId (ConnectionModel.Connection connectionId True def def (dst ^. withArrow) def) def
        Global.graph . Graph.connectionWidgetsMap . at dst ?= widgetId

batchConnectNodes :: OutPortRef -> InPortRef -> Command Global.State ()
batchConnectNodes src dst = BatchCmd.connectNodes src dst

