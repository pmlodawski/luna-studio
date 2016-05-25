module BatchConnector.Commands where

import           Utils.PreludePlus

import           Data.Binary                       (encode)
import qualified Data.Binary                       as Binary
import           Data.ByteString.Lazy.Char8        (pack)
import           Data.Int
import           Data.Map                          as Map
import           Data.UUID.Types                   (UUID)
import qualified Data.Sequence                     as Seq
import qualified Data.Text.Lazy                    as Text
import qualified Data.Text.Lazy                    as Text
import           Utils.Vector                      (Vector2 (..), x, y)

import           Batch.Workspace                   (Workspace)
import qualified Batch.Workspace                   as Workspace
import           BatchConnector.Connection         (WebMessage (..), sendRequest, sendRequests)

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

addNode :: Text -> NodeMeta -> Maybe NodeId -> Workspace -> UUID -> IO ()
addNode expression meta connectTo workspace uuid = sendRequest uuid $ (withLibrary workspace AddNode.Request) (AddNode.ExpressionNode $ Text.unpack expression) meta connectTo

createProject :: Text -> UUID -> IO ()
createProject name uuid = sendRequest uuid $ CreateProject.Request $ Text.unpack name

listProjects :: UUID -> IO ()
listProjects uuid = sendRequest uuid $ ListProjects.Request

createLibrary :: Text -> Text -> Workspace -> UUID -> IO ()
createLibrary name path workspace uuid = sendRequest uuid $ CreateLibrary.Request (workspace ^. Workspace.currentLocation . GraphLocation.projectId)
                                                                                  (Just $ Text.unpack name)
                                                                                  (Text.unpack path)
listLibraries :: ProjectId -> UUID -> IO ()
listLibraries projectId uuid = sendRequest uuid $ ListLibraries.Request projectId

getProgram :: Workspace -> UUID -> IO ()
getProgram workspace uuid = sendRequest uuid $ withLibrary workspace GetProgram.Request

updateNodeMeta :: NodeId -> NodeMeta -> Workspace -> UUID -> IO ()
updateNodeMeta nid nm workspace uuid = sendRequest uuid $ withLibrary workspace UpdateNodeMeta.Request nid nm

renameNode :: NodeId -> Text -> Workspace -> UUID -> IO ()
renameNode nid name w uuid = sendRequest uuid $ withLibrary w RenameNode.Request nid name

removeNode :: [NodeId] -> Workspace -> UUID ->  IO ()
removeNode nodeIds workspace uuid = sendRequest uuid $ withLibrary workspace RemoveNode.Request nodeIds

connectNodes :: OutPortRef -> InPortRef -> Workspace -> UUID -> IO ()
connectNodes src dst workspace uuid = sendRequest uuid $ (withLibrary workspace Connect.Request) src dst

disconnectNodes :: InPortRef -> Workspace -> UUID -> IO ()
disconnectNodes dst workspace uuid = sendRequest uuid $ withLibrary workspace Disconnect.Request dst

setDefaultValue :: AnyPortRef -> DefaultValue.PortDefault -> Workspace -> UUID -> IO ()
setDefaultValue portRef val workspace uuid = sendRequest uuid $ (withLibrary workspace SetDefaultValue.Request) portRef val

setInputNodeType :: NodeId -> Text -> Workspace -> UUID -> IO ()
setInputNodeType id tpe workspace uuid = sendRequest uuid $ (withLibrary workspace SetInputNodeType.Request) id (Text.unpack tpe)

