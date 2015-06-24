var THREE = require('three');
var $ = require('jquery');

var commonUniforms =  {
  camPan: {type: 'v2',value: new THREE.Vector2(0.0, 0.0)},
  camFactor: {type: 'f',value: 1.0},
  screenSize: {type: 'v2', value: new THREE.Vector2(1280.0, 800.0)},
  halfScreen: {type: 'v2', value: new THREE.Vector2(1280.0, 800.0)}
};

module.exports = {
  commonUniforms: commonUniforms,
  screenSize: commonUniforms.screenSize.value,
  halfScreen: commonUniforms.halfScreen,
  camFactor: commonUniforms.camFactor,
  camFactorBounds: [0.2, 4],
  camPan: commonUniforms.camPan.value
};

$(document).ready(function(){
  module.exports.htmlCanvasPan = $("#htmlcanvas-pan");
  module.exports.htmlCanvas = $("#htmlcanvas");
});

window.$$ = module.exports;