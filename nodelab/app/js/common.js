"use strict";

var commonUniforms =  {
  camFactor:  {type: 'f',  value: 1.0},
  antialias:  {type: 'i',  value: 0},
  objectMap:  {type: 'i',  value: 0},
  dpr:        { type: 'f',  value: 1.0 }, // TODO: js_devicePixelRatio
  aa:         { type: 'f',  value: 0.8 },
  zoomScaling:{ type: 'i',  value: 0 },
  connectionPen:{ type: 'i',  value: 1 }
};

module.exports = {
  commonUniforms: commonUniforms,
  camFactor:      commonUniforms.camFactor,
  scene:          undefined,
  camera:         undefined,
  renderer:       undefined,
  htmlCanvasPan:  undefined,
  htmlCanvas:     undefined,
  node_searcher:  undefined,
  websocket:      undefined,
  lastFactor:     1.0,
  registry:       {}
};

window.$$ = module.exports;
