"use strict";

var $$ = require('common');
var colors = require('colors');
var vs = require('shaders/connection.vert')();
var fs = require('shaders/connection.frag')();

var color = new THREE.Vector4(0.5, 0.5, 0.05, 0.6);

function Connection(widgetId, id, colorId) {
  var _this = this;
  this.id = id;
  this.geometry = new THREE.PlaneBufferGeometry(1.0, 10.0);

  var color = colors[colorId];

  this.uniforms = {
    color:      { type: 'v4', value: color },
    visible:    { type: 'f',  value: 0 },
    connecting: { type: 'i',  value: (widgetId == 3?1:0) },
    len:        { type: 'f',  value: 0 },
    focused:    { type: 'i',  value: 0 },
    objectId:   { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }

  };

  Object.keys($$.commonUniforms).forEach(function(k) {
    _this.uniforms[k] = $$.commonUniforms[k];
  });

  this.mesh = new THREE.Mesh(
    this.geometry,
    new THREE.ShaderMaterial({
      uniforms:       this.uniforms,
      vertexShader:   vs,
      fragmentShader: fs,
      transparent:    true,
      blending:       THREE.NormalBlending,
      side:           THREE.DoubleSide
    })
  );

  this.mesh.scale.x = 1;
  this.mesh.rotation.z = 0;
  this.mesh.position.z = 100;
}

Connection.prototype.setPos = function(x0, y0, x1, y1) {
  var dist = 0;
  var x = x1 - x0;
  var y = y1 - y0;
  var r = Math.sqrt(x * x + y * y);
  var x_r = x / r;
  var y_r = y / r;

  this.mesh.material.uniforms.len.value = r;

  this.mesh.scale.x = Math.max((r - 2 * dist), 0);
  var scale = this.mesh.scale.x / 2;

  this.mesh.rotation.z = Math.sign(y) * Math.acos(x_r);

  this.mesh.position.x = (dist * x_r) + x0 + x_r * scale;
  this.mesh.position.y = (dist * y_r) + y0 + y_r * scale;
};

Connection.prototype.show = function(colorId) {
  if (colorId !== undefined) {
    this.uniforms.color.value = colors[colorId];
  }
  this.mesh.material.uniforms.visible.value = 1;
};

Connection.prototype.hide = function() {
  this.mesh.material.uniforms.visible.value = 0;
};

Connection.prototype.setFocused = function(focused) {
  this.uniforms.focused.value = focused ? 1 : 0;
};


module.exports = Connection;
