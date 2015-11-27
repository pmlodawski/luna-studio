"use strict";

var $$           = require('common');
var vs           = require('shaders/sdf.vert')();
var fs           = require('shaders/slider.frag')();
var config       = require('config');

var createText   = require('bmfont').render,
    font         = require("font/LatoBlack-sdf"),
    textMaterial = require('font/text_material').hud,
    layoutText   = require('bmfont').layout;

var calculateTextWidth = function (txt) {
    return layoutText({font: font, text: txt}).width;
};

function Button(widgetId, width, height) {
  var _this = this;
  this.widgetId = widgetId;

  this.uniforms = {
    enabled:   { type: 'i', value: 1 },
    size:      { type: 'v2', value: new THREE.Vector2(width, height) },
    focus:     { type: 'i',  value: 0 },
    objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }
  };

  Object.keys($$.commonUniforms).forEach(function (k) {
    _this.uniforms[k]     = $$.commonUniforms[k];
  });

  this.mesh = new THREE.Group();

  var bgMesh = new THREE.PlaneBufferGeometry(1, 1);
  bgMesh.applyMatrix( new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0));

  this.bg = new THREE.Mesh(
      bgMesh,
      new THREE.ShaderMaterial( {
        uniforms:       this.uniforms,
        vertexShader:   vs,
        fragmentShader: fs,
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
}


Button.prototype.setEnabled = function (value) {
  this.uniforms.enabled.value = value?1:0;
};

Button.prototype.setLabel = function (text) {
  if (this.label) this.mesh.remove(this.label);

  var geometry = createText({
    text:  text,
    font:  font,
    align: 'center'
  });
  var textWidth = calculateTextWidth(text) * config.fontSize;

  var material = textMaterial();
  this.label = new THREE.Mesh(geometry, material);
  this.label.scale.multiplyScalar(config.fontSize);
  this.label.position.x = this.uniforms.size.value.x / 2.0 - textWidth / 2.0;
  this.label.position.y = 5  + this.uniforms.size.value.y / 2.0;
  this.label.position.z = 0;

  this.mesh.add(this.label);
};

module.exports = Button;
