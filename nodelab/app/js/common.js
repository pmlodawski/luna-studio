"use strict";

var commonUniforms =  {
  camFactor:    { type: 'f',  value: 1.0},
  antialias:    { type: 'i',  value: 0},
  objectMap:    { type: 'i',  value: 0},
  dpr:          { type: 'f',  value: 1.0 }, // TODO: js_devicePixelRatio
  aa:           { type: 'f',  value: 0.8 },
  zoomScaling:  { type: 'i',  value: 0 },
  connectionPen:{ type: 'i',  value: 0 },
  isConnecting: { type: 'i',  value: 0 }
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
  registry:       {},
  isGAEnabled:    function() { return !(localStorage.getItem('ga') === '0'); },
  enableGA:       function(val) { localStorage.setItem('ga', val?1:0); alert("Ok, Google Analytics will be " + (val?"enabled":"disabled") + " after you reload the page." ); }
};

window.$$ = module.exports;
