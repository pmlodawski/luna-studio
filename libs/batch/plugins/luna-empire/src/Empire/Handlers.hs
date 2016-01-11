module Empire.Handlers where

import           Prologue

import           Control.Monad.State                    (StateT)
import           Flowbox.Bus.BusT                       (BusT (..))
import           Empire.Env                             (Env)
import           Data.ByteString                        (ByteString)
import           Data.Map.Strict                        (Map)
import qualified Data.Map.Strict                        as Map
import qualified Empire.API.Topic                       as Topic

import qualified Empire.Server.Graph                    as Graph

type Handler = ByteString -> StateT Env BusT ()

handlersMap :: Map String Handler
handlersMap = Map.fromList
    [ (Topic.addNodeRequest,    Graph.handleAddNode)
    , (Topic.removeNodeRequest, Graph.handleRemoveNode)
    , (Topic.updateNodeMetaRequest, Graph.updateNodeMeta)
    ]
