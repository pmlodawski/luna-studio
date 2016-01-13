module Empire.API.Topic where

addNodeRequest         = "empire.graph.node.add.request" -- request update
addNodeUpdate          = "empire.graph.node.add.update" -- request update

nodeUpdate             = "empire.graph.node.update" -- async, no request

codeRequest            = "empire.graph.code.request"
codeUpdate             = "empire.graph.code.update" -- async, no request

graphRequest           = "empire.graph.graph.request"
graphUpdate            = "empire.graph.graph.update"

removeNodeRequest      = "empire.graph.node.remove.request"
nodeRemovedUpdate      = "empire.graph.node.remove.update"

updateNodeMetaRequest  = "empire.graph.node.updateMeta.request" -- request update
updateNodeMetaResponse = "empire.graph.node.updateMeta.update" -- request

connectRequest         = "empire.graph.connect.request" -- request simpleresponse
connectResponse        = "empire.graph.connect.update" -- request simpleresponse
disconnectRequest      = "empire.graph.disconnect.request" -- request simpleresponse
disconnectResponse     = "empire.graph.disconnect.update" -- request simpleresponse

createProjectRequest   = "empire.project.create.request" -- request update
createProjectResponse  = "empire.project.create.response" -- request update
listProjectsRequest    = "empire.project.list.request" -- request-response
listProjectsResponse   = "empire.project.list.response" -- request-response

createLibraryRequest   = "empire.library.create.request" -- request update
createLibraryResponse  = "empire.library.create.response" -- request update
listLibrariesRequest   = "empire.library.list.request" -- request-response
listLibrariesResponse  = "empire.library.list.response" -- request-response
