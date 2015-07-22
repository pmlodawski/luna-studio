"use strict";

var $$       = require('common');
var config   = require('config');

var createText   = require('bmfont').render;
var font         = require("font/LatoBlack-sdf");
var textMaterial = require('font/text_material').graph;

var vs = require('shaders/common.vert')();
var fs = require('shaders/button.frag')();

function Button(id, position, size, label) {
  var _this = this;

  this.id = id;
  this.position = position;
  this.labelText = label;
  this.size = size;

  this.uniforms = {
      state: { type: 'i', value: 1 }
  };

  this.attributes = {};

  Object.keys($$.commonUniforms).forEach(function(k) { // copy common uniforms
    _this.uniforms[k] = $$.commonUniforms[k];
  });

  var geometry = new THREE.PlaneGeometry(size.x, size.y);
  geometry.applyMatrix( new THREE.Matrix4().makeTranslation(size.x/2.0, size.y/2.0, 0) );

  this.mesh = new THREE.Mesh(
      geometry,
      new THREE.ShaderMaterial({
        uniforms:       this.uniforms,
        attributes:     this.attributes,
        vertexShader:   vs,
        fragmentShader: fs,
        transparent:    true,
        blending:       THREE.NormalBlending,
        side:           THREE.DoubleSide
      })
  );

  this.moveTo(position.x, position.y);

  this.updateLabel();
}

Button.prototype.moveTo = function(a, b) {
  var vec = new THREE.Vector2(a, b);

  this.position.x = vec.x;
  this.position.y = vec.y;

  this.mesh.position.x = vec.x;
  this.mesh.position.y = vec.y;
};


Button.prototype.updateLabel = function() {
  if (this.labelObject) this.mesh.remove(this.labelObject);

  var geom = createText({
    text: this.labelText,
    font: font,
    width: this.size.x/config.fontSize,
    align: 'center'
  });

  var obj = new THREE.Mesh(geom, textMaterial);
  obj.scale.multiplyScalar(config.fontSize);
  obj.position.z = 0.0001;
  obj.position.y = 4 + this.size.y / 2.0;

  this.labelObject = obj;
  this.mesh.add(obj);
};

Button.prototype.setState = function(state) {
    this.uniforms.state.value = state;
}

Button.prototype.label = function(text) {
  if (text !== undefined) {
    this.labelText = text;
    this.updateLabel();
  }
  return this.labelText;
};

module.exports = Button;
