module Event.Batch where

import           Utils.PreludePlus

import           Batch.Project
import           Batch.Library
import           Batch.Breadcrumbs
import           Object.Node
import qualified Generated.Proto.Interpreter.Interpreter.Value.Update as Value

data Event = ProjectsList [Project]
           | ProjectCreated Project
           | LibrariesList [Library]
           | LibraryCreated Library
           | WorkspaceCreated Breadcrumbs
           | NodeAdded Node
           | ValueUpdate Value.Update
           | UnknownEvent String
           | ParseError String
           deriving (Eq, Show)

instance PrettyPrinter Event where
    display = show
