module Event.Batch where

import Utils.PreludePlus

import Batch.Project
import Batch.Library
import Batch.Breadcrumbs
import Batch.Value
import Object.Node
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
           | NodesConnected
           | GraphViewFetched [Node] [(PortRef, PortRef)]
           | InterpreterGotProjectId (Maybe Int32)
           | SerializationModeInserted
           | ValueUpdate Int Value
           | CodeUpdate Text
           | RunFinished
           | UnknownEvent String
           | ParseError String
           | ConnectionDropped
           | ConnectionOpened
           deriving (Eq, Show)

instance PrettyPrinter Event where
    display = show
