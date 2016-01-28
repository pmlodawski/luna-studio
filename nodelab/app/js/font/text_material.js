"use strict";

var $$ = require('common')
  , fs = require('shaders/font.frag')()
  , vs = require('shaders/font.vert')();


THREE.ImageUtils.crossOrigin = '';
var fontTexture = THREE.ImageUtils.loadTexture(window.resourcesPath + '/font/LatoBlack-sdf.png');

var defaults = function (){
  return {
    uniforms: {
      opacity:   { type: 'f', value: 1 },
      smooth:    { type: 'f', value: 1/6 },
      map:       { type: 't', value: fontTexture },
      color:     { type: 'c', value: new THREE.Color(0x999999) },
      width:     { type: 'f', value: 0 },
      objectMap: $$.commonUniforms.objectMap,
      camFactor: $$.commonUniforms.camFactor,
      zoomScaling: { type: 'i', value: 0 },
    },
    vertexShader:   vs,
    fragmentShader: fs,
    transparent:    true,
    side:           THREE.DoubleSide,
    blending:       THREE.NormalBlending
  };
};



module.exports = { hud:   function () { return new THREE.ShaderMaterial(defaults() ); }
                 , graph: function () {
                     var graphOpts = defaults();
                     graphOpts.uniforms.zoomScaling.value = 1;
                     return new THREE.ShaderMaterial(graphOpts);
                   }
                 };
