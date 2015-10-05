"use strict";

var $$ = require('common');
var colors = require('colors');
var vs = require('shaders/port.vert')();
var fs = require('shaders/port.frag')();

var nodeRadius    = 30.0;

var colorFar     = colors[2];

var height = 30.0;
var width  = 30.0;
var halfWidth = width / 2.0;
var margin = 0.0;
var dist = nodeRadius + halfWidth + margin;

var nodeSize = 30.0;

function Port(id, widgetId, colorId, angle, out) {
  var _this = this;
  this.id = id;
  this.out = out;

  var color = colors[colorId];

  this.uniforms = {
    color:     { type: 'v4', value: color    },
    colorFar:  { type: 'v4', value: colorFar },
    focused:   { type: 'i',  value: 0        },
    mouseDist: { type: 'f',  value: 100000.0 },
    nodeSize:  { type: 'f',  value: nodeSize },
    portSize:  { type: 'f',  value: height   },
    objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }
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

Port.prototype.setExpandedPosition = function(left, top) {
  this.mesh.rotation.z = 0;
  this.mesh.position.x = left;
  this.mesh.position.y = top;
}

Port.prototype.setColor = function(color) {
  this.uniforms.color.value = color;
};

Port.prototype.updateMouseDist = function(mouseDist) {
  this.uniforms.mouseDist.value = mouseDist;
};

Port.prototype.setFocused = function(focused) {
  this.uniforms.focused.value = focused ? 1 : 0;
};


module.exports = Port;
