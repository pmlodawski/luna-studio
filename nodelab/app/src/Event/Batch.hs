module Event.Batch where

import Utils.PreludePlus

import Batch.Breadcrumbs
import Batch.Value
import Batch.RunStatus
import Empire.API.Data.Node (Node)
import Empire.API.Data.PortRef (OutPortRef, InPortRef)
import           Empire.API.Data.Project (ProjectId, Project)
import qualified Empire.API.Data.Project as Project
import           Empire.API.Data.Library (LibraryId, Library)
import qualified Empire.API.Data.Library as Library

import qualified Empire.API.Graph.AddNode           as AddNode
import qualified Empire.API.Graph.RemoveNode        as RemoveNode
import qualified Empire.API.Graph.GetProgram        as GetProgram
import qualified Empire.API.Graph.Connect           as Connect
import qualified Empire.API.Graph.Disconnect        as Disconnect
import qualified Empire.API.Graph.UpdateNodeMeta    as UpdateNodeMeta
import qualified Empire.API.Graph.NodeResultUpdate  as NodeResultUpdate
import qualified Empire.API.Graph.NodeUpdate        as NodeUpdate
import qualified Empire.API.Graph.CodeUpdate        as CodeUpdate
import Empire.API.JSONInstances ()

import Data.Text.Lazy    (Text)
import Data.Int
import Data.Aeson (ToJSON)

data Event = UnknownEvent String
           | NodeAdded                  AddNode.Update
           | NodeRemoved             RemoveNode.Update
           | ProgramFetched          GetProgram.Update
           | NodesConnected             Connect.Update
           | NodesDisconnected       Disconnect.Update
           | NodeMetaUpdated     UpdateNodeMeta.Update
           | NodeUpdated             NodeUpdate.Update
           | CodeUpdated             CodeUpdate.Update
           | NodeResultUpdated NodeResultUpdate.Update
           -- | ProjectsList [Project]
           -- | ProjectCreated Project
           -- | ProjectOpened Project
           -- | ProjectDoesNotExist
           -- | LibrariesList [Library]
           -- | LibraryCreated Library
           -- | ASTElementExists
           -- | ASTElementDoesNotExist
           -- | WorkspaceCreated Breadcrumbs
           -- | NodeModified
           -- | NodeDefaultUpdated
           -- | InterpreterGotProjectId (Maybe Int32)
           -- | SerializationModeInserted
           -- | ValueUpdate Int Value
           -- | CodeSet
           -- | CodeSetError String
           -- | RunFinished RunStatus
           -- | ParseError String
           | ConnectionDropped
           | ConnectionOpened
           deriving (Eq, Show, Generic)

instance PrettyPrinter Event where
    display = show

instance ToJSON Event
