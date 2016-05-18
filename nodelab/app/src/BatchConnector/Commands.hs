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

addNode :: Text -> NodeMeta -> Maybe Int -> Workspace -> IO ()
addNode expression meta connectTo workspace = sendRequest $ (withLibrary workspace AddNode.Request) (AddNode.ExpressionNode $ Text.unpack expression) meta connectTo

createProject :: Text -> Text -> IO ()
createProject name path = sendRequest $ CreateProject.Request (Just $ Text.unpack name) (Text.unpack path)

listProjects :: IO ()
listProjects = sendRequest $ ListProjects.Request

createLibrary :: Text -> Text -> Workspace -> IO ()
createLibrary name path w = sendRequest $ CreateLibrary.Request (w ^. Workspace.currentLocation . GraphLocation.projectId)
                                                                (Just $ Text.unpack name)
                                                                (Text.unpack path)

listLibraries :: ProjectId -> IO ()
listLibraries projectId = sendRequest $ ListLibraries.Request projectId

getProgram :: Workspace -> IO ()
getProgram workspace = sendRequest $ withLibrary workspace GetProgram.Request

updateNodeMeta :: NodeId -> NodeMeta -> Workspace -> IO ()
updateNodeMeta nid nm w = sendRequest $ withLibrary w UpdateNodeMeta.Request nid nm

renameNode :: NodeId -> Text -> Workspace -> IO ()
renameNode nid name w = sendRequest $ withLibrary w RenameNode.Request nid name

removeNode :: [NodeId] -> Workspace ->  IO ()
removeNode nodeIds workspace  = sendRequest $ withLibrary workspace RemoveNode.Request nodeIds

connectNodes :: OutPortRef -> InPortRef -> Workspace -> IO ()
connectNodes src dst workspace  = sendRequest $ (withLibrary workspace Connect.Request) src dst

disconnectNodes :: [(OutPortRef, InPortRef)] -> Workspace -> IO ()
disconnectNodes connections workspace = sendRequests $ (withLibrary workspace Disconnect.Request) <$> snd <$> connections

setDefaultValue :: AnyPortRef -> DefaultValue.PortDefault -> Workspace -> IO ()
setDefaultValue portRef val workspace = sendRequest $ (withLibrary workspace SetDefaultValue.Request) portRef val

setInputNodeType :: NodeId -> Text -> Workspace -> IO ()
setInputNodeType id tpe workspace  = sendRequest $ (withLibrary workspace SetInputNodeType.Request) id (Text.unpack tpe)

