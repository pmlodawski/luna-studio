"use strict";

var $$ = require('common')
  , fs = require('shaders/font.frag')()
  , vs = require('shaders/font.vert')();

module.exports = new THREE.ShaderMaterial({
  uniforms: {
    opacity:   { type: 'f', value: 1 },
    smooth:    { type: 'f', value: 1/4 },
    map:       { type: 't', value: THREE.ImageUtils.loadTexture('font/LatoBlack-sdf.png') },
    color:     { type: 'c', value: new THREE.Color("rgb(255, 255, 255)") },
    camFactor: $$.commonUniforms.camFactor
  },
  vertexShader:   vs,
  fragmentShader: fs,
  transparent:    true,
  side:           THREE.DoubleSide,
  blending:       THREE.NormalBlending
});
