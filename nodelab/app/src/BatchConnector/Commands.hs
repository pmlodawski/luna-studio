module BatchConnector.Commands where

import           Utils.PreludePlus

import           Data.ByteString.Lazy.Char8        (pack)
import qualified Data.Sequence                     as Seq
import           Data.Map                          as Map
import           Data.Int
import qualified Data.Text.Lazy                    as Text
import qualified Data.Text.Lazy                    as Text
import           Utils.Vector                      (Vector2(..), x, y)

import           Batch.Workspace                   (Workspace)
import qualified Batch.Workspace                   as Workspace
import           Batch.Expressions
import           BatchConnector.Connection         (sendMessage, sendMany, sendRequest, WebMessage(..))
import           Empire.API.Data.Node              (Node(..))
import qualified Empire.API.Data.Node              as Node
import qualified Empire.API.Data.Port              as Port
import           Empire.API.Data.PortRef           (InPortRef(..), OutPortRef(..))
import qualified Empire.API.Data.PortRef           as PortRef
import qualified Empire.API.Data.Connection        as Connection
import           Empire.API.Data.NodeMeta          (NodeMeta)
import qualified Empire.API.Data.NodeMeta          as NodeMeta
import           Empire.API.Data.Node              (NodeId)
import           Empire.API.Data.Port              (InPort(..))
import           Empire.API.Data.Project           (ProjectId, Project)
import qualified Empire.API.Data.Project           as Project
import           Empire.API.Data.GraphLocation     (GraphLocation)
import qualified Empire.API.Data.GraphLocation     as GraphLocation
import           Empire.API.Data.Library           (LibraryId, Library)
import qualified Empire.API.Data.Library           as Library
import qualified Empire.API.Data.DefaultValue      as DefaultValue
import qualified Empire.API.Topic                  as Topic

import qualified Empire.API.Graph.AddNode          as AddNode
import qualified Empire.API.Graph.Connect          as Connect
import qualified Empire.API.Graph.Disconnect       as Disconnect
import qualified Empire.API.Graph.RemoveNode       as RemoveNode
import qualified Empire.API.Graph.RenameNode       as RenameNode
import qualified Empire.API.Graph.UpdateNodeMeta   as UpdateNodeMeta
import qualified Empire.API.Graph.SetDefaultValue  as SetDefaultValue
import qualified Empire.API.Graph.GetProgram       as GetProgram
import qualified Empire.API.Project.CreateProject  as CreateProject
import qualified Empire.API.Project.ListProjects   as ListProjects
import qualified Empire.API.Library.CreateLibrary  as CreateLibrary
import qualified Empire.API.Library.ListLibraries  as ListLibraries


import Data.Binary (encode)
import qualified Data.Binary as Binary

withLibrary :: Workspace -> (GraphLocation -> a) -> a
withLibrary w f = f (w ^. Workspace.currentLocation)

addNode :: Workspace -> Text -> NodeMeta -> Int -> IO ()
addNode workspace expression meta tag = sendRequest topic body where
    topic = Topic.addNodeRequest
    body  = (withLibrary workspace AddNode.Request) (Text.unpack expression)
                                                   meta
                                                   tag

createProject :: Text -> Text -> IO ()
createProject name path = sendRequest topic body where
    topic = Topic.createProjectRequest
    body  = CreateProject.Request (Just $ Text.unpack name)
                                  (Text.unpack path)

listProjects :: IO ()
listProjects = sendRequest Topic.listProjectsRequest ListProjects.Request

createLibrary :: Workspace -> Text -> Text -> IO ()
createLibrary w name path = sendRequest topic body where
    topic = Topic.createLibraryRequest
    body  = CreateLibrary.Request (w ^. Workspace.currentLocation . GraphLocation.projectId)
                                  (Just $ Text.unpack name)
                                  (Text.unpack path)

listLibraries :: ProjectId -> IO ()
listLibraries projectId = sendRequest Topic.listLibrariesRequest $ ListLibraries.Request projectId

getProgram :: Workspace -> IO ()
getProgram workspace = sendRequest Topic.programRequest $ withLibrary workspace GetProgram.Request

updateNodeMeta :: Workspace -> NodeId -> NodeMeta -> IO ()
updateNodeMeta w nid nm = sendRequest Topic.updateNodeMetaRequest $ withLibrary w UpdateNodeMeta.Request nid nm

renameNode :: Workspace -> NodeId -> Text -> IO ()
renameNode w nid name = sendRequest Topic.renameNodeRequest $ withLibrary w RenameNode.Request nid name

removeNode :: Workspace -> NodeId -> IO ()
removeNode workspace nid = sendRequest topic body where
    topic = Topic.removeNodeRequest
    body  = withLibrary workspace RemoveNode.Request nid


connectNodes :: Workspace -> OutPortRef -> InPortRef -> IO ()
connectNodes workspace src dst = sendRequest topic body where
    topic = Topic.connectRequest
    body = (withLibrary workspace Connect.Request) (src ^. PortRef.srcNodeId)
                                                   (src ^. PortRef.srcPortId)
                                                   (dst ^. PortRef.dstNodeId)
                                                   (dst ^. PortRef.dstPortId)

disconnectMessage :: Workspace -> (OutPortRef, InPortRef) -> WebMessage
disconnectMessage workspace (src, dst) = WebMessage Topic.disconnectRequest $ encode body where
    body  = (withLibrary workspace Disconnect.Request) (dst ^. PortRef.dstNodeId)
                                                       (dst ^. PortRef.dstPortId)

disconnectNodes :: Workspace -> [(OutPortRef, InPortRef)] -> IO ()
disconnectNodes workspace connections = sendMany $ (disconnectMessage workspace) <$> connections

setDefaultValue :: Workspace -> InPortRef -> DefaultValue.PortDefault -> IO ()
setDefaultValue workspace inPortRef val = sendRequest topic body where
    topic = Topic.setDefaultValueRequest
    body = (withLibrary workspace SetDefaultValue.Request) inPortRef val

