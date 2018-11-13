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
      nodeEditorBaseGL.onEvent @_handleUIEvent

  open: (@uri) =>
    @backend.node.pushEvent(tag: "SetFile", path: @uri)
    @backend.code.pushInternalEvent(tag: "OpenFile", _path: @uri)

  _handleUIEvent: (path, event, target) =>
    if event.tag.endsWith "Event"
      evt = if event.tag != 'MouseEvent' then event else
        #TODO we have to rewrap this to avoid recursive fields, which freeze ghcjs's parser
        tag: event.constructor.name
        altKey: event.altKey
        bubbles: event.bubbles
        button: event.button
        buttons: event.buttons
        cancelBubble: event.cancelBubble
        cancelable: event.cancelable
        clientX: event.clientX
        clientY: event.clientY
        ctrlKey: event.ctrlKey
        defaultPrevented: event.defaultPrevented
        detail: event.detail
        eventPhase: event.eventPhase
        isTrusted: event.isTrusted
        layerX: event.layerX
        layerY: event.layerY
        metaKey: event.metaKey
        movementX: event.movementX
        movementY: event.movementY
        offsetX: event.offsetX
        offsetY: event.offsetY
        pageX: event.pageX
        pageY: event.pageY
        returnValue: event.returnValue
        screenX: event.screenX
        screenY: event.screenY
        shiftKey: event.shiftKey
        sourceCapabilities: event.sourceCapabilities
        timeStamp: event.timeStamp
        type_: event.type
        which: event.which
        x: event.x
        y: event.y
      base =
          tag:      evt.tag.substring(0, evt.tag.length - 5)
          contents: evt
      target ?= []
      @backend.node.pushViewEvent
          path:   path
          target: target
          base:   base
