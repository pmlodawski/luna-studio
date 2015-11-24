"use strict";

var $$           = require('common');
var vs           = require('shaders/sdf.vert')();
var fs           = require('shaders/radio.frag')();
var config       = require('config');

var createText   = require('bmfont').render,
    font         = require("font/LatoBlack-sdf"),
    textMaterial = require('font/text_material').hud;

function RadioButton(widgetId, width, height) {
  var _this = this;
  this.widgetId = widgetId;

  this.iconUniforms = {
    size:      { type: 'v2', value: new THREE.Vector2(height, height) },
    value:     { type: 'i',  value: 0 },
  };

  this.uniforms = {
    size:      { type: 'v2', value: new THREE.Vector2(width, height) },
    focus:     { type: 'i',  value: 0 },
    objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }
  };

  Object.keys($$.commonUniforms).forEach(function (k) {
    _this.uniforms[k]     = $$.commonUniforms[k];
    _this.iconUniforms[k] = $$.commonUniforms[k];
  });

  this.mesh = new THREE.Group();

  var bgMesh = new THREE.PlaneBufferGeometry(1, 1);
  bgMesh.applyMatrix( new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0));

  this.bg = new THREE.Mesh(
      bgMesh,
      new THREE.ShaderMaterial( {
        uniforms:       this.uniforms,
        vertexShader:   vs,
        fragmentShader: require('shaders/transparent.frag')(),
        transparent:    true,
        blending:       THREE.NormalBlending,
        side:           THREE.DoubleSide,
        derivatives:    true
    })
  );

  this.bg.position.z = 0;
  this.bg.scale.x    = width;
  this.bg.scale.y    = height;
  this.mesh.add(this.bg);

  this.icon = new THREE.Mesh(
      bgMesh,
      new THREE.ShaderMaterial( {
        uniforms:       this.iconUniforms,
        vertexShader:   vs,
        fragmentShader: fs,
        transparent:    true,
        blending:       THREE.NormalBlending,
        side:           THREE.DoubleSide,
        derivatives:    true
    })
  );

  this.icon.position.z = 0;
  this.icon.scale.x    = height;
  this.icon.scale.y    = height;

  this.mesh.add(this.icon);
}


RadioButton.prototype.setValue = function (value) {
  this.iconUniforms.value.value = value?1:0;
};

RadioButton.prototype.setFocus = function (value) {
  this.iconUniforms.focus.value = value?1:0;
};

RadioButton.prototype.setLabel = function (text) {
  if (this.label) this.mesh.remove(this.label);

  var geometry = createText({
    text:  text,
    font:  font,
    align: 'left'
  });

  var material = textMaterial();
  this.label = new THREE.Mesh(geometry, material);
  this.label.scale.multiplyScalar(config.fontSize);
  this.label.position.x = 12 + this.uniforms.size.value.y / 2.0;
  this.label.position.y = 5  + this.uniforms.size.value.y / 2.0;
  this.label.position.z = 0;

  this.mesh.add(this.label);
};

module.exports = RadioButton;
