import * as logger  from 'luna-logger'
import * as keypress from 'keypress.js'
baseglUI = require 'luna-basegl-ui'

import * as keymap from './keymap'


mountPoint = 'node-editor'

# TODO: We should never keep such functions attached to window.
window.listVisualizers = => [] #TODO
window.getInternalVisualizersPath = => '' #TODO
window.getLunaVisualizersPath = => '' #TODO
window.getInternalVisualizers = => {} #TODO
window.getLunaVisualizers = => [] #TODO
window.getProjectVisualizers = => [] #TODO
window.getImportedVisualizers = => {} #TODO
window.visualizerFramesManager = require './visualizers' #TODO


export class NodeEditor
  constructor: (@backend) ->
    logger.group 'Initializing BaseGL', =>
      baseglUI.install mountPoint, 'rsc/', (@nodeEditor) =>
        @backend.node.setView @nodeEditor
        logger.group 'Launching node backend', =>
          @backend.node.start()
      baseglUI.onEvent @_handleUIEvent
      @listener = new keypress.Listener()
      @_addShortcutListeners()

  _addShortcutListeners: =>
    s = @_shortcuts()
    for key, binding of keymap.keymap
      if s[binding]?
        @listener.register_combo
          keys: key
          on_keydown: s[binding]
          is_unordered: true
          is_solitary: true


  _pushShortcut: (name, arg = null) =>
    @backend.node.pushEvent
      _shortcut: name
      _arg: arg

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

  _shortcuts: =>
    f = @_pushShortcut
    'cancel': => f 'Cancel'
    'accept': => f 'Accept'
    # camera
    'center-graph': => f 'CenterGraph'
    'pan-down':     => f 'PanDown'
    'pan-left':     => f 'PanLeft'
    'pan-right':    => f 'PanRight'
    'pan-up':       => f 'PanUp'
    'reset-camera': => f 'ResetCamera'
    'reset-pan':    => f 'ResetPan'
    'reset-zoom':   => f 'ResetZoom'
    'zoom-in':      => f 'ZoomIn'
    'zoom-out':     => f 'ZoomOut'
    # clipboard
    'copy':  => f 'Copy'
    'cut':   => f 'Cut'
    'paste': => f 'Paste', atom.clipboard.read() #TODO
    # navigation
    'exit-graph':    => f 'ExitGraph'
    'go-cone-down':  => f 'GoConeDown'
    'go-cone-left':  => f 'GoConeLeft'
    'go-cone-right': => f 'GoConeRight'
    'go-cone-up':    => f 'GoConeUp'
    'go-down':       => f 'GoDown'
    'go-left':       => f 'GoLeft'
    'go-next':       => f 'GoNext'
    'go-prev':       => f 'GoPrev'
    'go-right':      => f 'GoRight'
    'go-up':         => f 'GoUp'
    # nodes
    'autolayout-all-nodes':        => f 'AutolayoutAllNodes'
    'autolayout-selected-nodes':   => f 'AutolayoutSelectedNodes'
    'close-visualization-preview': => f 'CloseVisualizationPreview'
    'collapse-to-function':        => f 'CollapseToFunction'
    'edit-selected-nodes':         => f 'EditSelectedNodes'
    'expand-selected-nodes':       => f 'ExpandSelectedNodes'
    'open-visualization-preview':  => f 'OpenVisualizationPreview'
    'remove-selected-nodes':       => f 'RemoveSelectedNodes'
    'select-all':                  => f 'SelectAll'
    'unfold-selected-nodes':       => f 'UnfoldSelectedNodes'
    'zoom-visualization':          => f 'ZoomVisualization'
    # undo/redo
    'core:redo': => f 'Redo'
    'core:undo': => f 'Undo'
    # MockMonads
    'mock-add-monad':    => f 'MockAddMonad'
    'mock-clear-monads': => f 'MockClearMonads'
    # searcher
    'searcher-open': (e)        =>
      scene = @nodeEditor._scene
      campos = scene.camera.position
      y = scene.height/2 + campos.y + (-scene.screenMouse.y + scene.height/2) * campos.z
      x = scene.width/2  + campos.x + (scene.screenMouse.x - scene.width/2) * campos.z
      f 'SearcherOpen', '(' + x + ',' + y + ')'
    'searcher-edit-expression': => f 'SearcherEditExpression'
    # debug
    'debug-layer-0': => f 'EnableDebugLayer', "0"
    'debug-layer-1': => f 'EnableDebugLayer', "1"
    'debug-layer-2': => f 'EnableDebugLayer', "3"
    'debug-layer-3': => f 'EnableDebugLayer', "2"
    'debug-layer-4': => f 'EnableDebugLayer', "4"
    'debug-layer-5': => f 'EnableDebugLayer', "5"
    'debug-layer-6': => f 'EnableDebugLayer', "6"
    'debug-layer-7': => f 'EnableDebugLayer', "7"
    'debug-layer-8': => f 'EnableDebugLayer', "8"
    'debug-layer-9': => f 'EnableDebugLayer', "9"
