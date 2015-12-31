module Event.Batch where

import Utils.PreludePlus

import Batch.Project
import Batch.Library
import Batch.Breadcrumbs
import Batch.Value
import Batch.RunStatus
import Empire.API.Data.Node (Node)
import Empire.API.Data.Connection (OutPortRef, InPortRef)
import Data.Text.Lazy    (Text)
import Data.Int

data Event = ProjectsList [Project]
           | ProjectCreated Project
           | ProjectOpened Project
           | ProjectDoesNotExist
           | LibrariesList [Library]
           | LibraryCreated Library
           | ASTElementExists
           | ASTElementDoesNotExist
           | WorkspaceCreated Breadcrumbs
           | NodeAdded Node
           | NodeRemoved
           | NodeModified
           | NodesConnected
           | NodesDisconnected
           | NodeDefaultUpdated
           | GraphViewFetched [Node] [(OutPortRef, InPortRef)]
           | InterpreterGotProjectId (Maybe Int32)
           | SerializationModeInserted
           | ValueUpdate Int Value
           | CodeUpdate Text
           | CodeSet
           | CodeSetError String
           | RunFinished RunStatus
           | UnknownEvent String
           | ParseError String
           | ConnectionDropped
           | ConnectionOpened
           deriving (Eq, Show)

instance PrettyPrinter Event where
    display = show
