{-# LANGUAGE Rank2Types #-}
module Event.Processors.Batch where

import           Utils.PreludePlus
import           Data.Binary (decode, Binary)
import qualified Data.Map.Lazy              as Map
import qualified Event.Event                as Event
import           Event.Connection           as Connection
import           Event.Batch                as Batch
import           BatchConnector.Connection  (WebMessage(..), ControlCode(..))
import           Empire.API.Topic    as Topic
import           Empire.API.Response as Response
import           Data.ByteString.Lazy.Char8  (ByteString)


process :: Event.Event -> Maybe Event.Event
process (Event.Connection (Message msg)) = Just $ Event.Batch $ processMessage msg
process _                                = Nothing

handle :: forall a. (Binary a, MessageTopic a) => (a -> Batch.Event) -> (String, (ByteString -> Batch.Event))
handle cons = (Topic.topic (undefined :: a), cons . decode)

handlers = Map.fromList [ handle NodeAdded
                        , handle AddNodeResponse
                        , handle NodeRemoved
                        , handle RemoveNodeResponse
                        , handle NodesConnected
                        , handle ConnectResponse
                        , handle NodesDisconnected
                        , handle DisconnectResponse
                        , handle NodeMetaUpdated
                        , handle NodeMetaResponse
                        , handle NodeUpdated
                        , handle ProgramFetched
                        , handle CodeUpdated
                        , handle NodeResultUpdated
                        , handle ProjectList
                        , handle ProjectCreated
                        , handle ProjectCreatedUpdate
                        , handle ProjectExported
                        , handle ProjectImported
                        , handle NodeRenamed
                        , handle NodeSearcherUpdated
                        , handle EmpireStarted
                        ]

processMessage :: WebMessage -> Batch.Event
processMessage (WebMessage topic bytes) = handler bytes where
    handler      = Map.findWithDefault defHandler topic handlers
    defHandler _ = UnknownEvent topic
processMessage (ControlMessage ConnectionTakeover) = ConnectionDropped
processMessage (ControlMessage Welcome)            = ConnectionOpened
