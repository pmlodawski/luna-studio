module BatchConnector.Commands where

import           Utils.PreludePlus

import           Data.Binary                       (encode)
import qualified Data.Binary                       as Binary
import           Data.ByteString.Lazy.Char8        (pack)
import           Data.Int
import           Data.Map                          as Map
import qualified Data.Sequence                     as Seq
import qualified Data.Text.Lazy                    as Text
import qualified Data.Text.Lazy                    as Text
import           Utils.Vector                      (Vector2 (..), x, y)

import           Batch.Workspace                   (Workspace)
import qualified Batch.Workspace                   as Workspace
import           BatchConnector.Connection         (WebMessage (..), sendMany, sendMessage, sendRequest)

import qualified Empire.API.Data.Connection        as Connection
import qualified Empire.API.Data.DefaultValue      as DefaultValue
import           Empire.API.Data.GraphLocation     (GraphLocation)
import qualified Empire.API.Data.GraphLocation     as GraphLocation
import           Empire.API.Data.Library           (Library, LibraryId)
import qualified Empire.API.Data.Library           as Library
import           Empire.API.Data.Node              (Node (..))
import           Empire.API.Data.Node              (NodeId)
import qualified Empire.API.Data.Node              as Node
import           Empire.API.Data.NodeMeta          (NodeMeta)
import qualified Empire.API.Data.NodeMeta          as NodeMeta
import           Empire.API.Data.Port              (InPort (..))
import qualified Empire.API.Data.Port              as Port
import           Empire.API.Data.PortRef           (InPortRef (..), OutPortRef (..), AnyPortRef (..))
import qualified Empire.API.Data.PortRef           as PortRef
import           Empire.API.Data.Project           (Project, ProjectId)
import qualified Empire.API.Data.Project           as Project
import qualified Empire.API.Topic                  as Topic

import qualified Empire.API.Graph.AddNode          as AddNode
import qualified Empire.API.Graph.Connect          as Connect
import qualified Empire.API.Graph.Disconnect       as Disconnect
import qualified Empire.API.Graph.GetProgram       as GetProgram
import qualified Empire.API.Graph.RemoveNode       as RemoveNode
import qualified Empire.API.Graph.RenameNode       as RenameNode
import qualified Empire.API.Graph.SetDefaultValue  as SetDefaultValue
import qualified Empire.API.Graph.SetInputNodeType as SetInputNodeType
import qualified Empire.API.Graph.UpdateNodeMeta   as UpdateNodeMeta
import qualified Empire.API.Library.CreateLibrary  as CreateLibrary
import qualified Empire.API.Library.ListLibraries  as ListLibraries
import qualified Empire.API.Project.CreateProject  as CreateProject
import qualified Empire.API.Project.ListProjects   as ListProjects



withLibrary :: Workspace -> (GraphLocation -> a) -> a
withLibrary w f = f (w ^. Workspace.currentLocation)

addNode :: Workspace -> Text -> NodeMeta -> Maybe Int -> IO ()
addNode workspace expression meta connectTo = sendRequest topic body where
    topic = Topic.addNodeRequest
    body  = (withLibrary workspace AddNode.Request) (AddNode.ExpressionNode $ Text.unpack expression) meta connectTo

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

removeNode :: Workspace -> [NodeId] -> IO ()
removeNode workspace nodeIds = sendRequest topic body where
    topic = Topic.removeNodeRequest
    body  = withLibrary workspace RemoveNode.Request nodeIds


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

setDefaultValue :: Workspace -> AnyPortRef -> DefaultValue.PortDefault -> IO ()
setDefaultValue workspace portRef val = sendRequest topic body where
    topic = Topic.setDefaultValueRequest
    body = (withLibrary workspace SetDefaultValue.Request) portRef val

setInputNodeType :: Workspace -> NodeId -> Text -> IO ()
setInputNodeType workspace id tpe = sendRequest topic body where
    topic = Topic.setInputNodeTypeRequest
    body = (withLibrary workspace SetInputNodeType.Request) id (Text.unpack tpe)

