"use strict";

var $$           = require('common');
var config       = require('config');

var createText   = require('bmfont').render,
    font         = require("font/LatoBlack-sdf"),
    textMaterial = require('font/text_material').hud,
    layoutText   = require('bmfont').layout;

var calculateTextWidth = function (txt) {
    return layoutText({font: font, text: txt}).width;
};

function Label(widgetId, width, height) {
  var _this = this;
  this.widgetId = widgetId;

  this.uniforms = {};

  Object.keys($$.commonUniforms).forEach(function (k) {
    _this.uniforms[k]     = $$.commonUniforms[k];
  });

  this.mesh = new THREE.Group();
}

Label.prototype.setLabel = function (text) {
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
  this.label.position.y = 15.0;

  this.mesh.add(this.label);
};

module.exports = Label;
