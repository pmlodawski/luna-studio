import 'setimmediate'

import * as logger       from 'luna-logger'

import * as analytics    from './analytics'
import * as codeCallback from './codeCallback'
import * as config       from './config'
import * as gzip         from './gzip'
import * as nodeCallback from './nodeCallback'
import * as uuid         from './uuid'
import * as Libs         from './libs'
import * as websocket    from './websocket'

websocketConfig = websocket: websocket()

###################
### Libs Config ###
###################

libConfig =
  nodeEditor :
    path : 'lib/node-editor.js'
    args :
      arg_url      : -> '/tmp/luna/Test/src/Main.luna' #TODO remove
      arg_mount    : -> 'node-editor' #TODO remove
      analytics    : analytics
      callback     : nodeCallback
      config       : config #TODO rename
      generateUUID : uuid.generateUUID
      gzip         : gzip
      init         : websocketConfig #TODO rename

  codeEditor :
    path : 'lib/text-editor.js'
    args :
      analytics : analytics
      callback  : codeCallback
      config    : config #TODO rename
      gzip      : gzip
      init      : websocketConfig #TODO rename

export initialize = =>
  libs = await Libs.load libConfig

  globalRegistry = {}

  return
    node:
      start:           libs.nodeEditor
      connector:       nodeCallback.connector
      setView:         nodeCallback.setNodeEditorView
      onNotification:  nodeCallback.onNotification
      pushViewEvent:   nodeCallback.view.pushEvent
      setEventFilter:  nodeCallback.setEventFilter
      onExpectedEvent: nodeCallback.onExpectedEvent
      pushEvent:       (msg) =>
        logger.info 'Sending [node]', msg
        nodeCallback.pushEvent msg
    code:
      start:               libs.codeEditor
      connect:             (connector)   => connector(globalRegistry)
      lex:                 (stack, data) => codeCallback.lex stack, data
      onInsertCode:        (callback)    => codeCallback.onInsertCode callback
      onInterpreterUpdate: (callback)    => codeCallback.onInterpreterUpdate callback
      onSetBuffer:         (callback)    => codeCallback.onSetBuffer callback
      onSetClipboard:      (callback)    => codeCallback.onSetClipboard callback
      pushDiffs:           (diffs)       => codeCallback.pushDiffs diffs
      onStatus:            (callback)    => codeCallback.onStatus callback
      pushInternalEvent:   (msg)         =>
        logger.info 'Sending [code]', msg
        codeCallback.pushInternalEvent msg
