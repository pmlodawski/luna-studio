module Empire.API.Graph.Topics where

import Prologue

addNode        = "empire.graph.node.add" -- request update
removeNode     = "empire.graph.node.remove" -- request simpleresponse
updateNode     = "empire.graph.node.update" -- async, no request
updateNodeMeta = "empire.graph.node.updateMeta" -- request update
connect        = "empire.graph.connect" -- request simpleresponse
disconnect     = "empire.graph.disconnect" -- request simpleresponse
createProject  = "empire.project.create" -- request update
listProjects   = "empire.project.list" -- request-response
createLibrary  = "empire.library.create" -- request update
listLibraries  = "empire.library.list" -- request-response
