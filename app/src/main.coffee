import 'setimmediate'

import * as analytics    from './analytics';
import * as callback     from './callback';
import * as codeCallback from './codeCallback';
import * as config       from './config';
import * as websocket    from './websocket';
import * as uuid         from './uuid';

init = websocket: websocket()
generateUUID = uuid.generateUUID
atomCallback = callback
n = nodeEditor({analytics, atomCallback, config, generateUUID, init});
atomCallbackTextEditor = codeCallback
c = codeEditor({analytics, atomCallbackTextEditor, config, init})

# n.start();
# callback.connector
# callback.setNodeEditorView
# callback.onNotification
# callback.pushEvent
# callback.view.pushEvent
# callback.setEventFilter
# callback.onExpectedEvent
