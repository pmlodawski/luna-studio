import _ from 'lodash';
import 'setimmediate';

import * as analytics from './analytics';
import * as config    from './config';
import * as websocket from './websocket';

var init = {websocket: websocket()}
console.log(config)
var n = nodeEditor({analytics, config, init});
n.start();
