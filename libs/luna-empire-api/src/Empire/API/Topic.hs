module Empire.API.Topic where

addNodeRequest         = "empire.graph.node.add.request"
addNodeUpdate          = "empire.graph.node.add.update"

removeNodeRequest      = "empire.graph.node.remove.request"
removeNodeUpdate       = "empire.graph.node.remove.update"

updateNodeMetaRequest  = "empire.graph.node.updateMeta.request"
updateNodeMetaUpdate   = "empire.graph.node.updateMeta.update"

connectRequest         = "empire.graph.connect.request"
connectUpdate          = "empire.graph.connect.update"

disconnectRequest      = "empire.graph.disconnect.request"
disconnectUpdate       = "empire.graph.disconnect.update"

setDefaultValueRequest = "empire.graph.node.defaultValue.request"
setDefaultValueUpdate  = "empire.graph.node.defaultValue.update"

programRequest         = "empire.graph.program.request"
programStatus          = "empire.graph.program.status"

nodeUpdate             = "empire.graph.node.update"   -- no request
nodeResultUpdate       = "empire.graph.result.update" -- no request
codeUpdate             = "empire.graph.code.update"   -- no request
graphUpdate            = "empire.graph.graph.update"  -- no request, for future use

createProjectRequest   = "empire.project.create.request"
createProjectUpdate    = "empire.project.create.update"

removeProjectRequest   = "empire.project.remove.request" -- for future use
removeProjectUpdate    = "empire.project.remove.update"

listProjectsRequest    = "empire.project.list.request"
listProjectsStatus     = "empire.project.list.status"

createLibraryRequest   = "empire.library.create.request"
createLibraryUpdate    = "empire.library.create.update"

removeLibraryRequest   = "empire.library.remove.request" -- for future use
removeLibraryUpdate    = "empire.library.remove.update"

listLibrariesRequest   = "empire.library.list.request"
listLibrariesStatus    = "empire.library.list.status"

logEnvDebug            = "empire.environment.debug"
