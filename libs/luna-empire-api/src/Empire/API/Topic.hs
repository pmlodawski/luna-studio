module Empire.API.Topic where

import Prologue

getGraphRequest        = "empire.graph.get.request" -- request-response
getGraphResponse       = "empire.graph.get.response" -- request-response

addNodeRequest         = "empire.graph.node.add.request" -- request update
removeNodeRequest      = "empire.graph.node.remove.request" -- request simpleresponse
nodeUpdate             = "empire.graph.node.update" -- async, no request
codeUpdate             = "empire.graph.code.update" -- async, no request

updateNodeMetaRequest  = "empire.graph.node.updateMeta.request" -- request update
updateNodeMetaResponse = "empire.graph.node.updateMeta.response" -- request

connectRequest         = "empire.graph.connec.request" -- request simpleresponse
disconnectRequest      = "empire.graph.disconnect.request" -- request simpleresponse

createProjectRequest   = "empire.project.create.request" -- request update
createProjectResponse  = "empire.project.create.response" -- request update
listProjectsRequest    = "empire.project.list.request" -- request-response
listProjectsResponse   = "empire.project.list.response" -- request-response

createLibraryRequest   = "empire.library.create.request" -- request update
createLibraryResponse  = "empire.library.create.response" -- request update
listLibrariesRequest   = "empire.library.list.request" -- request-response
listLibrariesResponse  = "empire.library.list.response" -- request-response
