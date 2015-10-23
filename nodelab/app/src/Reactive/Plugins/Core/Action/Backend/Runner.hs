module Reactive.Plugins.Core.Action.Backend.Runner where

import           Utils.PreludePlus

import           Object.Object (NodeId)
import           Object.Node   (Node)
import           Event.Event   (Event(Batch))
import qualified Event.Batch   as Batch
import           Batch.Value   (Value(..))
import           JS.NodeGraph  (setComputedValue, displayNodeVector)

import           BatchConnector.Monadic.Commands as BatchCmd

import           Reactive.Commands.Command (Command, performIO)
import           Reactive.State.Global     as Global
import           Reactive.State.Global     (State)

toAction :: Event Node -> Maybe (Command State ())
toAction (Batch (Batch.NodeAdded _))              = Just requestRerun
toAction (Batch  Batch.NodesConnected)            = Just requestRerun
toAction (Batch  Batch.NodesDisconnected)         = Just requestRerun
toAction (Batch  Batch.NodeRemoved)               = Just requestRerun
toAction (Batch  Batch.NodeModified)              = Just requestRerun
toAction (Batch  Batch.NodeDefaultUpdated)        = Just requestRerun
toAction (Batch (Batch.ValueUpdate nodeId value)) = Just $ updateValue nodeId value
toAction _                                        = Nothing

requestRerun :: Command State ()
requestRerun = zoom Global.workspace $ BatchCmd.runMain

updateValue :: NodeId -> Value -> Command State ()
updateValue nodeId (VectorValue vec) = performIO $ do
    setComputedValue nodeId "Vector"
    displayNodeVector nodeId (toList vec)
updateValue nodeId value = performIO $ setComputedValue nodeId (display value)
