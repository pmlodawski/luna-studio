module BatchConnector.Commands where

import           Utils.PreludePlus

import qualified Data.Text.Lazy                    as Text
import           Data.UUID.Types                   (UUID)
import qualified Data.UUID.Types                   as UUID

import           Batch.Workspace                   (Workspace)
import qualified Batch.Workspace                   as Workspace
import           BatchConnector.Connection         (sendRequest, sendUpdate)

import qualified Empire.API.Data.DefaultValue      as DefaultValue
import           Empire.API.Data.GraphLocation     (GraphLocation)
import qualified Empire.API.Data.GraphLocation     as GraphLocation
import           Empire.API.Data.Node              (NodeId)
import           Empire.API.Data.NodeMeta          (NodeMeta)
import           Empire.API.Data.PortRef           (AnyPortRef (..), InPortRef (..), OutPortRef (..))
import           Empire.API.Data.Project           (ProjectId)

import qualified Empire.API.Graph.AddNode          as AddNode
import qualified Empire.API.Graph.Collaboration    as Collaboration
import qualified Empire.API.Graph.Connect          as Connect
import qualified Empire.API.Graph.Disconnect       as Disconnect
import qualified Empire.API.Graph.DumpGraphViz     as DumpGraphViz
import qualified Empire.API.Graph.GetProgram       as GetProgram
import qualified Empire.API.Graph.RemoveNode       as RemoveNode
import qualified Empire.API.Graph.RenameNode       as RenameNode
import qualified Empire.API.Graph.SetDefaultValue  as SetDefaultValue
import qualified Empire.API.Graph.SetInputNodeType as SetInputNodeType
import qualified Empire.API.Graph.UpdateNodeMeta   as UpdateNodeMeta
import qualified Empire.API.Library.CreateLibrary  as CreateLibrary
import qualified Empire.API.Library.ListLibraries  as ListLibraries
import qualified Empire.API.Project.CreateProject  as CreateProject
import qualified Empire.API.Project.ExportProject  as ExportProject
import qualified Empire.API.Project.ImportProject  as ImportProject
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

collaborativeTouch :: Collaboration.ClientId ->[NodeId] -> Workspace -> IO ()
collaborativeTouch clientId ids workspace = sendUpdate $ (withLibrary workspace Collaboration.Update) clientId  $ Collaboration.Touch ids

collaborativeModify :: Collaboration.ClientId ->[NodeId] -> Workspace -> IO ()
collaborativeModify clientId ids workspace = sendUpdate $ (withLibrary workspace Collaboration.Update) clientId  $ Collaboration.Modify ids

cancelCollaborativeTouch :: Collaboration.ClientId -> [NodeId] -> Workspace -> IO ()
cancelCollaborativeTouch clientId ids workspace = sendUpdate $ (withLibrary workspace Collaboration.Update) clientId $ Collaboration.CancelTouch ids

exportProject :: ProjectId -> UUID -> IO ()
exportProject pid uuid = sendRequest uuid $ ExportProject.Request pid

importProject :: Text -> UUID -> IO ()
importProject payload uuid = sendRequest uuid $ ImportProject.Request payload

dumpGraphViz :: Workspace -> UUID -> IO ()
dumpGraphViz workspace uuid = sendRequest uuid $ withLibrary workspace DumpGraphViz.Request
