import 'setimmediate'
import * as path from 'path'

import * as analytics    from './analytics'
import * as callback     from './callback'
import * as codeCallback from './codeCallback'
import * as config       from './config'
import * as gzip         from './gzip'
import {NodeEditor}      from './NodeEditor'
import * as uuid         from './uuid'
import * as websocket    from './websocket'

window.listVisualizers = => [] #TODO
window.getInternalVisualizersPath = => '' #TODO
window.getLunaVisualizersPath = => '' #TODO
window.getInternalVisualizers = => {} #TODO
window.getLunaVisualizers = => [] #TODO

init = websocket: websocket()
generateUUID = uuid.generateUUID
atomCallback = callback
atomCallbackTextEditor = codeCallback

globalRegistry = {}

nodeBackend =
    start:           nodeEditor({analytics, atomCallback, config, generateUUID, gzip, init}).start
    connector:       callback.connector
    setView:         callback.setNodeEditorView
    onNotification:  callback.onNotification
    pushEvent:       callback.pushEvent
    pushViewEvent:   callback.view.pushEvent
    setEventFilter:  callback.setEventFilter
    onExpectedEvent: callback.onExpectedEvent

codeBackend =
    start:               codeEditor({analytics, atomCallbackTextEditor, config, gzip, init}).start
    connect:             (connector) => connector(globalRegistry)
    lex:                 (stack, data) => codeCallback.lex stack, data
    onInsertCode:        (callback) => codeCallback.onInsertCode callback
    onInterpreterUpdate: (callback) => codeCallback.onInterpreterUpdate callback
    onSetBuffer:         (callback) => codeCallback.onSetBuffer callback
    onSetClipboard:      (callback) => codeCallback.onSetClipboard callback
    pushDiffs:           (diffs)    => codeCallback.pushDiffs diffs
    pushInternalEvent:   (data)     => codeCallback.pushInternalEvent data
    onStatus:            (callback) => codeCallback.onStatus callback


class LunaStudio
    launch: =>
        @projectPath = '/home/pmlodawski/luna/projects/UnsavedLunaProject'
        codeBackend.connect nodeBackend.connector
        codeBackend.onStatus @__onStatus
        codeBackend.start()

    __onStatus: (act, arg0, arg1) =>
        console.log 'status:', { act, arg0, arg1 }
        switch act
            when 'Init'
                codeBackend.pushInternalEvent
                    tag: 'SetProject'
                    _path: @projectPath
            when 'ProjectSet'
                @openMain()

    openMain: =>
        mainLocation = path.join @projectPath, 'src', 'Main.luna'
        @nodeEditor ?= new NodeEditor mainLocation, nodeBackend


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
