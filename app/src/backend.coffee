import 'setimmediate'

import * as analytics    from './analytics'
import * as callback     from './callback'
import * as codeCallback from './codeCallback'
import * as config       from './config'
import * as gzip         from './gzip'
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
      atomCallback : callback #TODO rename
      config       : config #TODO rename
      generateUUID : uuid.generateUUID
      gzip         : gzip
      init         : websocketConfig #TODO rename

  codeEditor :
    path : 'lib/text-editor.js'
    args :
      analytics              : analytics
      atomCallbackTextEditor : codeCallback #TODO rename
      config                 : config #TODO rename
      gzip                   : gzip
      init                   : websocketConfig #TODO rename

export initialize = =>
  libs = await Libs.load libConfig

  globalRegistry = {}

  return
    node:
      start:           libs.nodeEditor
      connector:       callback.connector
      setView:         callback.setNodeEditorView
      onNotification:  callback.onNotification
      pushEvent:       callback.pushEvent
      pushViewEvent:   callback.view.pushEvent
      setEventFilter:  callback.setEventFilter
      onExpectedEvent: callback.onExpectedEvent
    code:
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
