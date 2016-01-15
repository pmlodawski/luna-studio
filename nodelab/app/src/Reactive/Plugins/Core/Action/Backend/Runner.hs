module Reactive.Plugins.Core.Action.Backend.Runner where

import           Utils.PreludePlus

import qualified Object.Widget.Node as Model
import           Event.Event   (Event(Batch))
import qualified Event.Batch   as Batch
import           Batch.Value   (Value(..))
import qualified Data.Text.Lazy as Text
-- import           JS.NodeGraph  (setComputedValue, displayNodeVector)

import           BatchConnector.Monadic.Commands as BatchCmd

import           Reactive.Commands.Command (Command, performIO)
import           Reactive.Commands.Graph (nodeIdToWidgetId)
import qualified Reactive.Commands.UIRegistry as UICmd
import           Reactive.State.Global     as Global
import           Reactive.State.Global     (State)
import           Empire.API.Data.Node (NodeId)

toAction :: Event -> Maybe (Command State ())
-- toAction (Batch (Batch.NodeAdded _))              = Just requestRerun
-- toAction (Batch  Batch.NodesConnected)            = Just requestRerun
-- toAction (Batch  Batch.NodesDisconnected)         = Just requestRerun
-- toAction (Batch  Batch.NodeRemoved)               = Just requestRerun
-- toAction (Batch  Batch.NodeModified)              = Just requestRerun
-- toAction (Batch  Batch.NodeDefaultUpdated)        = Just requestRerun
-- toAction (Batch (Batch.ValueUpdate nodeId value)) = Just $ updateValue nodeId value
toAction _                                        = Nothing

-- requestRerun :: Command State ()
-- requestRerun = zoom Global.workspace $ BatchCmd.runMain
--
-- updateValue :: NodeId -> Value -> Command State ()
-- updateValue nodeId (VectorValue vec) = do
--     id <- zoom Global.uiRegistry $ nodeIdToWidgetId nodeId
--     forM_ id $ \id -> zoom Global.uiRegistry $ UICmd.update id $ Model.value .~ (Text.pack $ display $ toList vec)
--     performIO $ putStrLn "Runner.hs: display node vector value"
--     -- setComputedValue nodeId "Vector"
--     -- displayNodeVector nodeId (toList vec)
-- updateValue nodeId value = do
--     id <- zoom Global.uiRegistry $ nodeIdToWidgetId nodeId
--     forM_ id $ \id -> zoom Global.uiRegistry $ UICmd.update id $ Model.value .~ (Text.pack $ display value)
