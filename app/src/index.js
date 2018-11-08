import _ from 'lodash';
import 'setimmediate';

import * as analytics    from './analytics';
import * as config       from './config';
import * as websocket    from './websocket';
import * as atomCallback from './callback';
import * as uuid from './uuid';

var init = {websocket: websocket()}
generateUUID = uuid.generateUUID
var n = nodeEditor({analytics, atomCallback, config, generateUUID, init});
n.start();
