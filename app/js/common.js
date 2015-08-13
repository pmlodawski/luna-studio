"use strict";

var commonUniforms =  {
  // camPan:     {type: 'v2', value: new THREE.Vector2(0.0, 0.0)},
  camFactor:  {type: 'f',  value: 1.0},
  // screenSize: {type: 'v2', value: new THREE.Vector2(1280.0, 800.0)},
  antialias:  {type: 'i',  value: 0},
  objectMap:  {type: 'i',  value: 1}
};

module.exports = {
  commonUniforms: commonUniforms,
  screenSize:     new THREE.Vector2(1280.0, 800.0),
  camFactor:      commonUniforms.camFactor,
  camPan:         new THREE.Vector2(0.0, 0.0),
  scene:          undefined,
  camera:         undefined,
  renderer:       undefined,
  htmlCanvasPan:  undefined,
  htmlCanvas:     undefined,
  node_searcher:  undefined,
  registry:       {}
};

$(document).ready(function(){
  module.exports.htmlCanvasPan = $("#htmlcanvas-pan");
  module.exports.htmlCanvas    = $("#htmlcanvas");
});

window.$$ = module.exports;
