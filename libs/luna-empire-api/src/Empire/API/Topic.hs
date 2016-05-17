{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Empire.API.Topic where

import           Prologue

import qualified Empire.API.Graph.AddNode            as AddNode
import qualified Empire.API.Graph.CodeUpdate         as CodeUpdate
import qualified Empire.API.Graph.Connect            as Connect
import qualified Empire.API.Graph.Disconnect         as Disconnect
import qualified Empire.API.Graph.GetProgram         as GetProgram
import qualified Empire.API.Graph.NodeResultUpdate   as NodeResultUpdate
import qualified Empire.API.Graph.NodeSearcherUpdate as NodeSearcherUpdate
import qualified Empire.API.Graph.NodeUpdate         as NodeUpdate
import qualified Empire.API.Graph.RemoveNode         as RemoveNode
import qualified Empire.API.Graph.RenameNode         as RenameNode
import qualified Empire.API.Graph.SetDefaultValue    as SetDefaultValue
import qualified Empire.API.Graph.SetInputNodeType   as SetInputNodeType
import qualified Empire.API.Graph.UpdateNodeMeta     as UpdateNodeMeta
import qualified Empire.API.Library.CreateLibrary    as CreateLibrary
import qualified Empire.API.Library.ListLibraries    as ListLibraries
import qualified Empire.API.Project.CreateProject    as CreateProject
import qualified Empire.API.Project.ListProjects     as ListProjects
import qualified Empire.API.Graph.TypeCheck          as Typecheck
import qualified Empire.API.Graph.DumpGraphViz       as DumpGraphViz
import           Empire.API.Response (Response)

type Topic = String

class MessageTopic a where
  topic :: a -> Topic

request  = ".request"
response = ".response"
update   = ".update"

addNode                = "empire.graph.node.add"
addNodeRequest         = "empire.graph.node.add.request"
addNodeResponse        = "empire.graph.node.add.response" -- data Response a = OK Request | Error Request Message
addNodeUpdate          = "empire.graph.node.add.update"   -- async
instance MessageTopic AddNode.Request              where topic _ = addNode <> request
instance MessageTopic (Response AddNode.Request a) where topic _ = addNode <> response
instance MessageTopic AddNode.Update               where topic _ = addNode <> update

removeNode             = "empire.graph.node.remove"
removeNodeRequest      = "empire.graph.node.remove.request"
removeNodeUpdate       = "empire.graph.node.remove.update"
instance MessageTopic RemoveNode.Request              where topic _ = removeNode <> request
instance MessageTopic (Response RemoveNode.Request a) where topic _ = removeNode <> response
instance MessageTopic RemoveNode.Update               where topic _ = removeNode <> update

updateNodeMeta         = "empire.graph.node.updateMeta"
updateNodeMetaRequest  = "empire.graph.node.updateMeta.request"
updateNodeMetaResponse = "empire.graph.node.updateMeta.response"
updateNodeMetaUpdate   = "empire.graph.node.updateMeta.update"
instance MessageTopic UpdateNodeMeta.Request               where topic _ = updateNodeMeta <> request
instance MessageTopic (Response UpdateNodeMeta.Request a)  where topic _ = updateNodeMeta <> response
instance MessageTopic UpdateNodeMeta.Update                where topic _ = updateNodeMeta <> update

renameNode             = "empire.graph.node.rename"
renameNodeRequest      = "empire.graph.node.rename.request"
renameNodeResponse     = "empire.graph.node.rename.response"
renameNodeUpdate       = "empire.graph.node.rename.update"

instance MessageTopic RenameNode.Request              where topic _ = renameNode <> request
instance MessageTopic (Response RenameNode.Request a) where topic _ = renameNode <> response
instance MessageTopic RenameNode.Update               where topic _ = renameNode <> update

connect                = "empire.graph.connect"
connectRequest         = "empire.graph.connect.request"
connectUpdate          = "empire.graph.connect.update"
instance MessageTopic Connect.Request              where topic _ = connect <> request
instance MessageTopic (Response Connect.Request a) where topic _ = connect <> response
instance MessageTopic Connect.Update               where topic _ = connect <> update

disconnect             = "empire.graph.disconnect"
disconnectRequest      = "empire.graph.disconnect.request"
disconnectUpdate       = "empire.graph.disconnect.update"
instance MessageTopic Disconnect.Request              where topic _ = disconnect <> request
instance MessageTopic (Response Disconnect.Request a) where topic _ = disconnect <> response
instance MessageTopic Disconnect.Update               where topic _ = disconnect <> update

setDefaultValue        = "empire.graph.node.defaultValue"
setDefaultValueRequest = "empire.graph.node.defaultValue.request"
setDefaultValueUpdate  = "empire.graph.node.defaultValue.update"
instance MessageTopic SetDefaultValue.Request              where topic _ = setDefaultValue <> request
instance MessageTopic (Response SetDefaultValue.Request a) where topic _ = setDefaultValue <> response
-- TODO: UPDATE

setInputNodeTypeRequest = "empire.graph.node.inputNodeType.request"
setInputNodeTypeUpdate  = "empire.graph.node.inputNodeType.update"

program                = "empire.graph.program"
programRequest         = "empire.graph.program.request"
programStatus          = "empire.graph.program.response"
instance MessageTopic GetProgram.Request  where topic _ = program <> request
instance MessageTopic (Response GetProgram.Request a) where topic _ = program <> response

nodeUpdate             = "empire.graph.node.update"   -- no request
instance MessageTopic NodeUpdate.Update  where topic _ = nodeUpdate
nodeResultUpdate       = "empire.graph.result.update" -- no request
instance MessageTopic NodeResultUpdate.Update  where topic _ = nodeResultUpdate
codeUpdate             = "empire.graph.code.update"   -- no request
instance MessageTopic CodeUpdate.Update  where topic _ = codeUpdate
graphUpdate            = "empire.graph.graph.update"  -- no request, for future use

createProject          = "empire.project.create"
createProjectRequest   = "empire.project.create.request"
createProjectUpdate    = "empire.project.create.update"
instance MessageTopic CreateProject.Request              where topic _ = createProject <> request
instance MessageTopic (Response CreateProject.Request a) where topic _ = createProject <> response
instance MessageTopic CreateProject.Update               where topic _ = createProject <> update

-- removeProjectRequest   = "empire.project.remove.request" -- for future use
-- removeProjectUpdate    = "empire.project.remove.update"

listProjects           = "empire.project.list"
listProjectsRequest    = "empire.project.list.request"
listProjectsStatus     = "empire.project.list.response"

instance MessageTopic ListProjects.Request              where topic _ = listProjects <> request
instance MessageTopic (Response ListProjects.Request a) where topic _ = listProjects <> response

createLibrary          = "empire.library.create"
createLibraryRequest   = "empire.library.create.request"
createLibraryUpdate    = "empire.library.create.update"
instance MessageTopic CreateLibrary.Request              where topic _ = createLibrary <> request
instance MessageTopic (Response CreateLibrary.Request a) where topic _ = createLibrary <> response
instance MessageTopic CreateLibrary.Update               where topic _ = createLibrary <> update

-- removeLibraryRequest   = "empire.library.remove.request" -- for future use
-- removeLibraryUpdate    = "empire.library.remove.update"

listLibraries          = "empire.library.list"
listLibrariesRequest   = "empire.library.list.request"
listLibrariesStatus    = "empire.library.list.status"
instance MessageTopic ListLibraries.Request              where topic _ = listLibraries <> request
instance MessageTopic (Response ListLibraries.Request a) where topic _ = listLibraries <> response


nodeSearcherDataUpdate = "empire.graph.nodeSearcher.update"

logEnvDebug            = "empire.environment.debug"

logEnvDebugGraphViz    = "empire.environment.debug.graphviz.request"
instance MessageTopic DumpGraphViz.Request  where topic _ = logEnvDebugGraphViz

typecheck              = "empire.environment.debug.typecheck.request"
instance MessageTopic Typecheck.Request  where topic _ = typecheck
instance MessageTopic (Response Typecheck.Request a) where topic _ = "empire.environment.debug.typecheck" <> response

controlEmpireStarted   = "empire.control.started.status"

