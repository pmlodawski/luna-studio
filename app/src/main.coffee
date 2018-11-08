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


init = websocket: websocket()
generateUUID = uuid.generateUUID
atomCallback = callback
n = nodeEditor({analytics, atomCallback, config, generateUUID, init})
atomCallbackTextEditor = codeCallback
c = codeEditor({analytics, atomCallbackTextEditor, config, gzip, init})

class LunaStudio
    launch: =>
        @projectPath = '/home/pmlodawski/luna/projects/UnsavedLunaProject'
        codeCallback.onStatus @__onStatus

    __onStatus: (act, arg0, arg1) =>
        console.log 'status:', { act, arg0, arg1 }
        switch act
            when 'Init'
                codeCallback.pushInternalEvent
                    tag: 'SetProject'
                    _path: @projectPath
            when 'ProjectSet'
                @openMain()

    openMain: =>
        mainLocation = path.join @projectPath, 'src', 'Main.luna'
        @nodeEditor ?= new NodeEditor


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
