"use strict";

var $$ = require('common');
var vs = require('shaders/triangle_port.vert')();
var fs = require('shaders/triangle_port.frag')();

var triangleRatio = 1.5;
var size          = 9;
var distFromRim   = 3;
var nodeRadius    = 30.0;


// var inputColor  = new THREE.Vector4(0xFF, 0x99, 0x33, 0xAA).divideScalar(0xFF);
var inputColor   = new THREE.Vector4(0x00, 0x99, 0x99, 0xAA).divideScalar(0xFF);
var outputColor  = new THREE.Vector4(0xBB, 0x33, 0x00, 0xAA).divideScalar(0xFF);
var colorFar     = new THREE.Vector4(0.2, 0.2, 0.2, 0.6);

var triangleHeight = size * Math.sqrt(3.0) / 2.0;
var halfHeight = triangleHeight / 2.0;
var dist  = nodeRadius + halfHeight + distFromRim;

function Port(id, angle, out) {
  var _this = this;
  this.id = id;
  this.out = out;

  var outputPort = [ new THREE.Vector3( halfHeight,                         0.0, 0.0),
                     new THREE.Vector3(-halfHeight, size *  triangleRatio * 0.5, 0.0),
                     new THREE.Vector3(-halfHeight, size * -triangleRatio * 0.5, 0.0)];

  var inputPort  = [ new THREE.Vector3(-halfHeight,                         0.0, 0.0),
                     new THREE.Vector3( halfHeight, size * -triangleRatio * 0.5, 0.0),
                     new THREE.Vector3( halfHeight, size *  triangleRatio * 0.5, 0.0)];

  var portVert = out ? outputPort  : inputPort;
  var color    = out ? outputColor : inputColor;

  this.attributes = {
    posL: {
      type: 'v3',
      value: [
        new THREE.Vector3(0.0, 1.0, 1.0),
        new THREE.Vector3(0.0, 1.0, 1.0),
        new THREE.Vector3(1.0, 1.0, 0.0)
      ]
    },
    posR: {
      type: 'v3',
      value: [
        new THREE.Vector3(0.0, 1.0, 1.0),
        new THREE.Vector3(1.0, 1.0, 0.0),
        new THREE.Vector3(0.0, 1.0, 1.0)
      ]
    },
    posB: {
      type: 'v3',
      value: [
        new THREE.Vector3(1.0, 1.0, 0.0),
        new THREE.Vector3(0.0, 1.0, 1.0),
        new THREE.Vector3(0.0, 1.0, 1.0)
      ]
    }

  };

  this.uniforms = {
    color:     { type: 'v4', value: color },
    colorFar:  { type: 'v4', value: colorFar },
    mouseDist: { type: 'f',  value: 100000.0 }
  };

  Object.keys($$.commonUniforms).forEach(function(k) {
    _this.uniforms[k] = $$.commonUniforms[k];
  });

  this.geometry = new THREE.Geometry();
  portVert.forEach(function(vert) {
    _this.geometry.vertices.push(vert);
  });
  this.geometry.faces.push(new THREE.Face3(0, 1, 2));
  this.mesh = new THREE.Mesh(
			this.geometry,
			new THREE.ShaderMaterial( {
        uniforms:       this.uniforms,
        attributes:     this.attributes,
				vertexShader:   vs,
				fragmentShader: fs,
				transparent:    true,
				blending:       THREE.NormalBlending
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
