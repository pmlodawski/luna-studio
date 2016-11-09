module Event.Batch where

import           Utils.PreludePlus

import qualified Empire.API.Control.EmpireStarted      as EmpireStarted
import qualified Empire.API.Graph.AddNode              as AddNode
import qualified Empire.API.Graph.CodeUpdate           as CodeUpdate
import qualified Empire.API.Graph.Collaboration        as Collaboration
import qualified Empire.API.Graph.Connect              as Connect
import qualified Empire.API.Graph.Disconnect           as Disconnect
import qualified Empire.API.Graph.GetProgram           as GetProgram
import qualified Empire.API.Graph.NodeResultUpdate     as NodeResultUpdate
import qualified Empire.API.Graph.NodeSearcherUpdate   as NodeSearcherUpdate
import qualified Empire.API.Graph.NodeUpdate           as NodeUpdate
import qualified Empire.API.Graph.RemoveNode           as RemoveNode
import qualified Empire.API.Graph.RenameNode           as RenameNode
import qualified Empire.API.Graph.UpdateNodeExpression as UpdateNodeExpression
import qualified Empire.API.Graph.UpdateNodeMeta       as UpdateNodeMeta
import           Empire.API.JSONInstances              ()
import qualified Empire.API.Project.CreateProject      as CreateProject
import qualified Empire.API.Project.ExportProject      as ExportProject
import qualified Empire.API.Project.ImportProject      as ImportProject
import qualified Empire.API.Project.ListProjects       as ListProjects

import           Data.Aeson                            (ToJSON)

data Event = UnknownEvent String
           | AddNodeResponse                           AddNode.Response
           | NodeAdded                                 AddNode.Update
           | RemoveNodeResponse                     RemoveNode.Response
           | NodeRemoved                            RemoveNode.Update
           | ProgramFetched                         GetProgram.Response
           | NodesConnected                            Connect.Update
           | ConnectResponse                           Connect.Response
           | NodesDisconnected                      Disconnect.Update
           | DisconnectResponse                     Disconnect.Response
           | NodeMetaUpdated                    UpdateNodeMeta.Update
           | NodeMetaResponse                   UpdateNodeMeta.Response
           | NodeRenamed                            RenameNode.Update
           | NodeRenameResponse                     RenameNode.Response
           | NodeUpdated                            NodeUpdate.Update
           | UpdateNodeExpressionResponse UpdateNodeExpression.Response
           | CodeUpdated                            CodeUpdate.Update
           | NodeResultUpdated                NodeResultUpdate.Update
           | ProjectList                          ListProjects.Response
           | ProjectCreated                      CreateProject.Response
           | ProjectCreatedUpdate                CreateProject.Update
           | ProjectExported                     ExportProject.Response
           | ProjectImported                     ImportProject.Response
           | NodeSearcherUpdated            NodeSearcherUpdate.Update
           | CollaborationUpdate                 Collaboration.Update
           | EmpireStarted                       EmpireStarted.Status
           | ConnectionDropped
           | ConnectionOpened
           deriving (Eq, Show, Generic)

instance ToJSON Event
