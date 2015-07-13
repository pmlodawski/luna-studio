"use strict";

// var $$       = require('common');
// var features = require('features');
// var config   = require('config');

var vs = require('shaders/common.vert')();
var fs = require('shaders/port.frag')();


var magic = 15 * Math.sqrt(3) / 4;
var dist  = 30 + (magic / 2) + 2.24;

function Port(id, angle, out) {
  var geometry;
  this.id = id;
  this.out = out;

  var inputColor = new THREE.Vector4(0,   0.5, 0, 1);
  var outputColor= new THREE.Vector4(0.7, 0,   0, 1);

  var outputPort = [ new THREE.Vector3( magic,       0.0, 0.0),
                     new THREE.Vector3(-magic, 15 *  0.5, 0.0),
                     new THREE.Vector3(-magic, 15 * -0.5, 0.0)];

  var inputPort  = [ new THREE.Vector3( magic, 15 *  0.5, 0.0),
                     new THREE.Vector3(-magic,       0.0, 0.0),
                     new THREE.Vector3( magic, 15 * -0.5, 0.0)];

  var portVert = out ? outputPort  : inputPort;
  var color    = out ? outputColor : inputColor;

  geometry = new THREE.Geometry();
  portVert.forEach( function(vert) {
    geometry.vertices.push(vert);
  });
  geometry.faces.push(new THREE.Face3(0, 1, 2));
  this.mesh = new THREE.Mesh(
			geometry,
			new THREE.ShaderMaterial( {
				uniforms: {
					color: { type: 'v4', value: color }
				},
				vertexShader:   vs,
				fragmentShader: fs,
				transparent: true,
				blending: THREE.NormalBlending
  	})
	);

  this.mesh.position.z = 0.0001;
  this.setAngle(angle);
}

Port.prototype.setAngle = function(angle) {
  this.angle = angle;

  this.mesh.position.x = Math.cos(angle) * dist;
  this.mesh.position.y = Math.sin(angle) * dist;

  this.mesh.rotation.z = angle;
};

module.exports = Port;
