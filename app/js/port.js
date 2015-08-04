"use strict";

var $$ = require('common');
var vs = require('shaders/port.vert')();
var fs = require('shaders/port.frag')();

var triangleRatio = 1.5;
var size          = 9;
var distFromRim   = 3;
var nodeRadius    = 30.0;


// var inputColor  = new THREE.Vector4(0xFF, 0x99, 0x33, 0xAA).divideScalar(0xFF);
var inputColor   = new THREE.Vector4(0x00, 0x99, 0x99, 0xAA).divideScalar(0xFF);
var outputColor  = new THREE.Vector4(0xBB, 0x33, 0x00, 0xAA).divideScalar(0xFF);
var colorFar     = new THREE.Vector4(0.2, 0.2, 0.2, 0.6);

var height = 12.0;
var width  = 12.0;
var halfWidth = width / 2.0;
var margin = 0.0;
var dist = nodeRadius + halfWidth + margin;

var nodeSize = 30.0;

function Port(id, angle, out) {
  var _this = this;
  this.id = id;
  this.out = out;

  var color    = out ? outputColor : inputColor;

  this.uniforms = {
    color:     { type: 'v4', value: color },
    colorFar:  { type: 'v4', value: colorFar },
    mouseDist: { type: 'f',  value: 100000.0 },
    nodeSize:  { type: 'f',  value: nodeSize },
    portSize:  { type: 'f',  value: height }
  };

  Object.keys($$.commonUniforms).forEach(function(k) {
    _this.uniforms[k] = $$.commonUniforms[k];
  });

  this.mesh = new THREE.Mesh(
      new THREE.PlaneBufferGeometry(width, height),
			new THREE.ShaderMaterial( {
        uniforms:       this.uniforms,
				vertexShader:   vs,
				fragmentShader: fs,
				transparent:    true,
				blending:       THREE.NormalBlending,
        side:           THREE.DoubleSide
  	})
	);

  this.mesh.position.z = 0;
  this.setAngle(angle);
}

Port.prototype.setAngle = function(angle) {
  this.angle = angle;

  this.mesh.position.x = Math.cos(angle) * dist;
  this.mesh.position.y = Math.sin(angle) * dist;

  this.mesh.rotation.z = angle;
};

Port.prototype.setColor = function(color) {
  this.uniforms.color.value = color;
};

Port.prototype.updateMouseDist = function(mouseDist) {
  this.uniforms.mouseDist.value = mouseDist;
};

module.exports = Port;
