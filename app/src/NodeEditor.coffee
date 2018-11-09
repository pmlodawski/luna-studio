import * as logger  from 'luna-logger'

nodeEditorBaseGL = require 'luna-basegl-ui'

mountPoint = 'node-editor'

# TODO: We should never keep such functions attached to window.
window.listVisualizers = => [] #TODO
window.getInternalVisualizersPath = => '' #TODO
window.getLunaVisualizersPath = => '' #TODO
window.getInternalVisualizers = => {} #TODO
window.getLunaVisualizers = => [] #TODO
window.getProjectVisualizers = => [] #TODO
window.getImportedVisualizers = => {} #TODO

export class NodeEditor
  constructor: (@backend) ->
    logger.group 'Initializing BaseGL', =>
      nodeEditorBaseGL.install mountPoint, 'rsc/', (ne) =>
        @backend.node.setView ne
        logger.group 'Launching node backend', =>
          @backend.node.start()

  open: (@uri) =>
    @backend.node.pushEvent(tag: "SetFile", path: @uri)
    @backend.code.pushInternalEvent(tag: "OpenFile", _path: @uri)
