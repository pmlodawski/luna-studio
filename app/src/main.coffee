import 'setimmediate'
import * as path from 'path'

import * as analytics    from './analytics'
import * as callback     from './callback'
import * as codeCallback from './codeCallback'
import * as config       from './config'
import * as Enum         from './enum'
import * as gzip         from './gzip'
import * as Libs         from './Libs'
import * as uuid         from './uuid'
import * as websocket    from './websocket'

import * as logger       from 'luna-logger'

import {NodeEditor}      from './NodeEditor'

# TODO: We should never keep such functions attached to window.
window.listVisualizers = => [] #TODO
window.getInternalVisualizersPath = => '' #TODO
window.getLunaVisualizersPath = => '' #TODO
window.getInternalVisualizers = => {} #TODO
window.getLunaVisualizers = => [] #TODO
window.getProjectVisualizers = => [] #TODO
window.getImportedVisualizers = => {} #TODO

websocketConfig = websocket: websocket()

###################
### Libs Config ###
###################

libConfig =
  nodeEditor :
    path : 'lib/node-editor.js'
    args :
      arg_url      : -> '/tmp/luna/Test/src/Main.luna'
      arg_mount    : -> 'node-editor'
      analytics    : analytics
      atomCallback : callback
      config       : config
      generateUUID : uuid.generateUUID
      gzip         : gzip
      init         : websocketConfig

  codeEditor : 
    path : 'lib/text-editor.js'
    args : 
      analytics              : analytics
      atomCallbackTextEditor : codeCallback
      config                 : config
      gzip                   : gzip
      init                   : websocketConfig

############
### Main ###
############

# TODO: This should be obtained from the backend, not hardcoded here
messages = Enum.make(
  'Init', 
  'ProjectSet', 
  'FileOpened',
  'SetProject',
  'GetBuffer'
)

main = () -> 
  libs = await Libs.load libConfig

  globalRegistry = {}

  nodeBackend =
    start:           libs.nodeEditor
    connector:       callback.connector
    setView:         callback.setNodeEditorView
    onNotification:  callback.onNotification
    pushEvent:       callback.pushEvent
    pushViewEvent:   callback.view.pushEvent
    setEventFilter:  callback.setEventFilter
    onExpectedEvent: callback.onExpectedEvent

  codeBackend =
    start:               libs.codeEditor
    connect:             (connector)   => connector(globalRegistry)
    lex:                 (stack, data) => codeCallback.lex stack, data
    onInsertCode:        (callback)    => codeCallback.onInsertCode callback
    onInterpreterUpdate: (callback)    => codeCallback.onInterpreterUpdate callback
    onSetBuffer:         (callback)    => codeCallback.onSetBuffer callback
    onSetClipboard:      (callback)    => codeCallback.onSetClipboard callback
    pushDiffs:           (diffs)       => codeCallback.pushDiffs diffs
    pushInternalEvent:   (data)        => codeCallback.pushInternalEvent data
    onStatus:            (callback)    => codeCallback.onStatus callback

  class LunaStudio
    launch: =>
      @projectPath = '/tmp/luna/Test'
      codeBackend.connect nodeBackend.connector
      codeBackend.onStatus @__onMessage
      nodeBackend.onNotification (msg) ->
        logger.warning 'Unhandled', msg
      codeBackend.start()

    # TODO: This design is to be changed. Having always 3 variables for 
    #       different purposes is very bad.
    __onMessage: (act, arg0, arg1) =>
      logger.group ('Received ' + act), =>
        logger.info 'args', { arg0, arg1 }
        switch act
          when messages.Init then @sendMessage
              tag: messages.SetProject
              _path: @projectPath
          when messages.ProjectSet then @openMain()
          when messages.FileOpened then @sendMessage
              tag: messages.GetBuffer
              _path: arg0

    sendMessage: (msg) -> 
      logger.info 'Sending', msg
      codeBackend.pushInternalEvent msg

    openMain: => logger.group 'Launching Node Editor', =>
      mainLocation = path.join @projectPath, 'src', 'Main.luna'
      # TODO: NodeEditor should open and initialize immediately.
      #       New file should be provided on demand. Initialization of 
      #       WebGL etc when openning file is a very bad idea.
      @nodeEditor ?= new NodeEditor mainLocation, nodeBackend, codeBackend

  logger.group 'launching Luna Studio', =>
    ls = new LunaStudio
    ls.launch()

  # n.start();
  # callback.connector
  # callback.setNodeEditorView
  # callback.onNotification
  # callback.pushEvent
  # callback.view.pushEvent
  # callback.setEventFilter
  # callback.onExpectedEvent



main()
