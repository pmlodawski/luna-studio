"use strict";

var $$ = require('common')
  , fs = require('shaders/font.frag')()
  , vs = require('shaders/font.vert')();

var defaults = function(){
  return {
    uniforms: {
      opacity:   { type: 'f', value: 1 },
      smooth:    { type: 'f', value: 1/6 },
      map:       { type: 't', value: THREE.ImageUtils.loadTexture('font/LatoBlack-sdf.png') },
      color:     { type: 'c', value: new THREE.Color(0x999999) },
      width:     { type: 'f', value: 0 },
      objectMap: $$.commonUniforms.objectMap,
      camFactor: $$.commonUniforms.camFactor,
      zoomScaling: { type: 'i', value: 0}
    },
    vertexShader:   vs,
    fragmentShader: fs,
    transparent:    true,
    side:           THREE.DoubleSide,
    blending:       THREE.NormalBlending
  };
};

var graphOpts = defaults();
graphOpts.uniforms.zoomScaling.value = 1;

var graph = new THREE.ShaderMaterial(graphOpts);
var hud   = new THREE.ShaderMaterial(defaults());

module.exports = {hud: hud, graph: graph};
