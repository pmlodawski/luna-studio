module Empire.API.Topic where

import Prologue

getGraphRequest       = "empire.graph.get.request" -- request-response
addNodeRequest        = "empire.graph.node.add.request" -- request update
removeNodeRequest     = "empire.graph.node.remove.request" -- request simpleresponse
nodeUpdate            = "empire.graph.node.update" -- async, no request
updateNodeMetaRequest = "empire.graph.node.updateMeta.request" -- request update
connectRequest        = "empire.graph.connec.request" -- request simpleresponse
disconnectRequest     = "empire.graph.disconnect.request" -- request simpleresponse
createProjectRequest  = "empire.project.create.request" -- request update
listProjectsRequest   = "empire.project.list.request" -- request-response
createLibraryRequest  = "empire.library.create.request" -- request update
listLibrariesRequest  = "empire.library.list.request" -- request-response
