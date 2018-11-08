import 'setimmediate'

import * as analytics from './analytics';
import * as config    from './config';
import * as websocket from './websocket';
import * as atomCallback from './callback';
import * as uuid from './uuid';

console.log "WORKING!6"

init = websocket: websocket()
generateUUID = uuid.generateUUID
n = nodeEditor({analytics, atomCallback, config, generateUUID, init});
n.start();
