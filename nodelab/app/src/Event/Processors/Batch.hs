module Event.Processors.Batch where

import           Utils.PreludePlus
import           Data.Binary (decode)

import qualified Event.Event                as Event
import           Event.Connection           as Connection
import           Event.Batch                as Batch
import           BatchConnector.Connection  (WebMessage(..), ControlCode(..))
import           Empire.API.Topic as Topic


process :: Event.Event -> Maybe Event.Event
process (Event.Connection (Message msg)) = Just $ Event.Batch $ processMessage msg
process _                                = Nothing

processMessage :: WebMessage -> Batch.Event
processMessage (WebMessage topic bytes)
    | topic == Topic.addNodeUpdate          = NodeAdded           $ decode bytes
    | topic == Topic.removeNodeUpdate       = NodeRemoved         $ decode bytes
    | topic == Topic.connectUpdate          = NodesConnected      $ decode bytes
    | topic == Topic.disconnectUpdate       = NodesDisconnected   $ decode bytes
    | topic == Topic.updateNodeMetaUpdate   = NodeMetaUpdated     $ decode bytes
    | topic == Topic.nodeUpdate             = NodeUpdated         $ decode bytes
    | topic == Topic.programStatus          = ProgramFetched      $ decode bytes
    | topic == Topic.codeUpdate             = CodeUpdated         $ decode bytes
    | topic == Topic.nodeResultUpdate       = NodeResultUpdated   $ decode bytes
    | topic == Topic.listProjectsStatus     = ProjectList         $ decode bytes
    | topic == Topic.createProjectUpdate    = ProjectCreated      $ decode bytes
    | topic == Topic.renameNodeUpdate       = NodeRenamed         $ decode bytes
    | topic == Topic.nodeSearcherDataUpdate = NodeSearcherUpdated $ decode bytes
    | topic == Topic.controlEmpireStarted   = EmpireStarted       $ decode bytes
    | otherwise                             = UnknownEvent topic

processMessage (ControlMessage ConnectionTakeover) = ConnectionDropped
processMessage (ControlMessage Welcome)            = ConnectionOpened

