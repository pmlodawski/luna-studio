"use strict";

var commonUniforms =  {
  camFactor:  {type: 'f',  value: 1.0},
  antialias:  {type: 'i',  value: 0},
  objectMap:  {type: 'i',  value: 1}
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
  registry:       {}
};

window.$$ = module.exports;
