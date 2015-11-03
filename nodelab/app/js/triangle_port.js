"use strict";

var $$ = require('common');
var vs = require('shaders/triangle_port.vert')();
var fs = require('shaders/triangle_port.frag')();

var inputColor   = new THREE.Vector4(0x00, 0x99, 0x99, 0xAA).divideScalar(0xFF);
var outputColor  = new THREE.Vector4(0xBB, 0x33, 0x00, 0xAA).divideScalar(0xFF);
var colorFar     = new THREE.Vector4(0.2, 0.2, 0.2, 0.6);

var dist = require('geometries/port').dist;

function Port(id, angle, out) {
  var _this = this;
  this.id = id;
  this.out = out;

  var portGeom = out ? require('geometries/port').out  : require('geometries/port').in;
  var color    = out ? outputColor : inputColor;

  this.uniforms = {
    color:     { type: 'v4', value: color },
    colorFar:  { type: 'v4', value: colorFar },
    mouseDist: { type: 'f',  value: 100000.0 }
  };

  Object.keys($$.commonUniforms).forEach(function (k) {
    _this.uniforms[k] = $$.commonUniforms[k];
  });

  this.mesh = new THREE.Mesh(
			portGeom,
      new THREE.ShaderMaterial( {
        uniforms:       this.uniforms,
        vertexShader:   vs,
        attributes:     ['posL', 'posR', 'posB'],
        fragmentShader: fs,
        transparent:    true,
        blending:       THREE.NormalBlending,
        side:           THREE.DoubleSide
      })
	);

  this.mesh.position.z = 0;
  this.setAngle(angle);
}

Port.prototype.setAngle = function (angle) {
  this.angle = angle;

  this.mesh.position.x = Math.cos(angle) * dist;
  this.mesh.position.y = Math.sin(angle) * dist;

  this.mesh.rotation.z = angle;
};

Port.prototype.setColor = function (color) {
  this.uniforms.color.value = color;
};

Port.prototype.updateMouseDist = function (mouseDist) {
  this.uniforms.mouseDist.value = mouseDist;
};

module.exports = Port;
