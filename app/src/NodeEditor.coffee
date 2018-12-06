import * as logger  from 'luna-logger'
baseglUI = require 'luna-basegl-ui'
import {Keyboard} from './keyboard'

mountPointName = 'node-editor'

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
  constructor: (@lunaStudio, @backend) ->
    logger.group 'Initializing BaseGL', =>
      @keyboard = new Keyboard @__shortcuts()
      baseglUI.install mountPointName, 'rsc/', @keyboard, (@nodeEditor) =>
        @backend.node.setView @nodeEditor
        logger.group 'Launching node backend', =>
          @backend.node.start()
      baseglUI.onEvent @_handleUIEvent

  __pushShortcutEvent: (name, arg = null) =>
    @backend.node.pushEvent
      _shortcut: name
      _arg: arg

  __pushSearcherEvent: (name, arg = null) =>
    @backend.node.pushEvent
      tag: name
      contents: arg if arg?

  setFile: (@path) =>
    @backend.node.pushEvent(tag: "SetFile", path: @path)

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

  __shortcuts: =>
    shortcut = @__pushShortcutEvent
    searcher = @__pushSearcherEvent
    default:
      'cancel': => shortcut 'Cancel'
      'accept': => shortcut 'Accept'
      'open':   => @lunaStudio.openProjectDialog()
      # camera
      'center-graph': => shortcut 'CenterGraph'
      'pan-down':     => shortcut 'PanDown'
      'pan-left':     => shortcut 'PanLeft'
      'pan-right':    => shortcut 'PanRight'
      'pan-up':       => shortcut 'PanUp'
      'reset-camera': => shortcut 'ResetCamera'
      'reset-pan':    => shortcut 'ResetPan'
      'reset-zoom':   => shortcut 'ResetZoom'
      'zoom-in':      => shortcut 'ZoomIn'
      'zoom-out':     => shortcut 'ZoomOut'
      # clipboard
      'copy':  => shortcut 'Copy'
      'cut':   => shortcut 'Cut'
      'paste': => shortcut 'Paste', atom.clipboard.read() #TODO
      # navigation
      'exit-graph':    => shortcut 'ExitGraph'
      'go-cone-down':  => shortcut 'GoConeDown'
      'go-cone-left':  => shortcut 'GoConeLeft'
      'go-cone-right': => shortcut 'GoConeRight'
      'go-cone-up':    => shortcut 'GoConeUp'
      'go-down':       => shortcut 'GoDown'
      'go-left':       => shortcut 'GoLeft'
      'go-next':       => shortcut 'GoNext'
      'go-prev':       => shortcut 'GoPrev'
      'go-right':      => shortcut 'GoRight'
      'go-up':         => shortcut 'GoUp'
      # nodes
      'autolayout-all-nodes':        => shortcut 'AutolayoutAllNodes'
      'autolayout-selected-nodes':   => shortcut 'AutolayoutSelectedNodes'
      'close-visualization-preview': => shortcut 'CloseVisualizationPreview'
      'collapse-to-function':        => shortcut 'CollapseToFunction'
      'edit-selected-nodes':         => shortcut 'EditSelectedNodes'
      'expand-selected-nodes':       => shortcut 'ExpandSelectedNodes'
      'open-visualization-preview':  => shortcut 'OpenVisualizationPreview'
      'remove-selected-nodes':       => shortcut 'RemoveSelectedNodes'
      'select-all':                  => shortcut 'SelectAll'
      'unfold-selected-nodes':       => shortcut 'UnfoldSelectedNodes'
      'zoom-visualization':          => shortcut 'ZoomVisualization'
      # undo/redo
      'redo': => shortcut 'Redo'
      'undo': => shortcut 'Undo'
      # MockMonads
      'mock-add-monad':    => shortcut 'MockAddMonad'
      'mock-clear-monads': => shortcut 'MockClearMonads'
      # searcher
      'searcher-open': (e)        =>
        scene = @nodeEditor.scene
        campos = scene.camera.position
        y = scene.height/2 + campos.y + (-scene.screenMouse.y + scene.height/2) * campos.z
        x = scene.width/2  + campos.x + (scene.screenMouse.x - scene.width/2) * campos.z
        shortcut 'SearcherOpen', '(' + x + ',' + y + ')'
      'searcher-edit-expression': => shortcut 'SearcherEditExpression'
      # debug
      'debug-layer-0': => shortcut 'EnableDebugLayer', "0"
      'debug-layer-1': => shortcut 'EnableDebugLayer', "1"
      'debug-layer-2': => shortcut 'EnableDebugLayer', "3"
      'debug-layer-3': => shortcut 'EnableDebugLayer', "2"
      'debug-layer-4': => shortcut 'EnableDebugLayer', "4"
      'debug-layer-5': => shortcut 'EnableDebugLayer', "5"
      'debug-layer-6': => shortcut 'EnableDebugLayer', "6"
      'debug-layer-7': => shortcut 'EnableDebugLayer', "7"
      'debug-layer-8': => shortcut 'EnableDebugLayer', "8"
      'debug-layer-9': => shortcut 'EnableDebugLayer', "9"
    searcher:
      'searcher-accept-0'     : => searcher 'HintShortcut', '0'
      'searcher-accept-1'     : => searcher 'HintShortcut', '1'
      'searcher-accept-2'     : => searcher 'HintShortcut', '2'
      'searcher-accept-3'     : => searcher 'HintShortcut', '3'
      'searcher-accept-4'     : => searcher 'HintShortcut', '4'
      'searcher-accept-5'     : => searcher 'HintShortcut', '5'
      'searcher-accept-6'     : => searcher 'HintShortcut', '6'
      'searcher-accept-7'     : => searcher 'HintShortcut', '7'
      'searcher-accept-8'     : => searcher 'HintShortcut', '8'
      'searcher-accept-9'     : => searcher 'HintShortcut', '9'
      'searcher-accept-input' : => searcher 'AcceptInput'
      'searcher-move-down'    : => searcher 'MoveDown'
      'searcher-accept'       : => searcher 'Accept'
      'searcher-tab-pressed'  : => searcher 'TabPressed'
      'searcher-move-up'      : => searcher 'MoveUp'
