"use strict";

var $$ = require('common')
  , fs = require('shaders/font.frag')()
  , vs = require('shaders/font.vert')();

var defaults = function(){
  return {
    uniforms: {
      opacity:   { type: 'f', value: 1 },
      smooth:    { type: 'f', value: 1/4 },
      map:       { type: 't', value: THREE.ImageUtils.loadTexture('font/LatoBlack-sdf.png') },
      color:     { type: 'c', value: new THREE.Color("rgb(255, 255, 255)") },
      camFactor: { type: 'f', value: 1 }
    },
    vertexShader:   vs,
    fragmentShader: fs,
    transparent:    true,
    side:           THREE.DoubleSide,
    blending:       THREE.NormalBlending
  };
};

var graphOpts = defaults();
graphOpts.uniforms.camFactor = $$.commonUniforms.camFactor;

var graph = new THREE.ShaderMaterial(graphOpts);
var hud   = new THREE.ShaderMaterial(defaults());

module.exports = {hud: hud, graph: graph};