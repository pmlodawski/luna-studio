module Empire.Handlers where

import           Prologue

import           Control.Monad.State   (StateT)
import           Data.ByteString       (ByteString)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import qualified Empire.API.Topic      as Topic
import           Empire.Env            (Env)
import qualified Empire.Server.Graph   as Graph
import qualified Empire.Server.Library as Library
import qualified Empire.Server.Project as Project
import           Flowbox.Bus.BusT      (BusT (..))

type Handler = ByteString -> StateT Env BusT ()

handlersMap :: Map String Handler
handlersMap = Map.fromList
    [ (Topic.addNodeRequest,         Graph.handleAddNode)
    , (Topic.removeNodeRequest,      Graph.handleRemoveNode)
    , (Topic.updateNodeMetaRequest,  Graph.handleUpdateNodeMeta)
    , (Topic.renameNodeRequest,      Graph.handleRenameNode)
    , (Topic.connectRequest,         Graph.handleConnect)
    , (Topic.disconnectRequest,      Graph.handleDisconnect)
    , (Topic.setDefaultValueRequest, Graph.handleSetDefaultValue)
    , (Topic.programRequest,         Graph.handleGetProgram)
    , (Topic.logEnvDebugGraphViz,    Graph.handleDumpGraphViz)
    , (Topic.createProjectRequest,   Project.handleCreateProject)
    , (Topic.listProjectsRequest,    Project.handleListProjects)
    , (Topic.createLibraryRequest,   Library.handleCreateLibrary)
    , (Topic.listLibrariesRequest,   Library.handleListLibraries)
    ]
