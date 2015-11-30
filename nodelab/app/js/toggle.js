"use strict";

var $$ = require('common');
var vs = require('shaders/sdf.vert')();
var fs = require('shaders/generic_bg.frag')();
var fs2 = require('shaders/toggle.frag')();
var config = require('config');

var createText   = require('bmfont').render,
    font         = require("font/LatoBlack-sdf"),
    textMaterial = require('font/text_material').hud;

function Toggle(widgetId, width, height) {
  var _this = this;
  this.widgetId = widgetId;
  this.width    = width;
  this.height   = height;

  this.bgUniforms = {
    size:      { type: 'v2', value: new THREE.Vector2(width, height) },
    focus:     { type: 'i',  value: 0 },
    objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }
  };

  this.uniforms = {
    size:      { type: 'v2', value: new THREE.Vector2(width, height) },
    value:     { type: 'i',  value: 0 },
  };

  Object.keys($$.commonUniforms).forEach(function (k) {
    _this.uniforms[k]   = $$.commonUniforms[k];
    _this.bgUniforms[k] = $$.commonUniforms[k];
  });

  this.mesh = new THREE.Group();
  this.makeBg();
  this.makeIndicator();
}

Toggle.prototype.makeBg = function() {
  var bgMesh = new THREE.PlaneBufferGeometry(1, 1);
  bgMesh.applyMatrix( new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0));

  this.bg = new THREE.Mesh(
      bgMesh,
      new THREE.ShaderMaterial( {
        uniforms:       this.bgUniforms,
        vertexShader:   vs,
        fragmentShader: fs,
        transparent:    true,
        blending:       THREE.NormalBlending,
        side:           THREE.DoubleSide,
        derivatives:    true
    })
  );
  this.bg.position.z = 0;
  this.bg.scale.x    = this.width;
  this.bg.scale.y    = this.height;

  this.mesh.add(this.bg);
};

Toggle.prototype.makeIndicator = function() {
  var bgMesh = new THREE.PlaneBufferGeometry(1, 1);
  bgMesh.applyMatrix( new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0));

  this.indicator = new THREE.Mesh(
      bgMesh,
      new THREE.ShaderMaterial( {
        uniforms:       this.uniforms,
        vertexShader:   vs,
        fragmentShader: fs2,
        transparent:    true,
        blending:       THREE.NormalBlending,
        side:           THREE.DoubleSide,
        derivatives:    true
    })
  );
  this.indicator.position.z  = 0;
  this.indicator.position.x  = this.width  * 0.8 - this.height / 2.0;
  this.indicator.position.y  = this.height * 0.1;
  this.uniforms.size.value.x = this.indicator.scale.x = this.width  * 0.2;
  this.uniforms.size.value.y = this.indicator.scale.y = this.height * 0.8;

  this.mesh.add(this.indicator);
};

Toggle.prototype.setValue = function (value) {
  this.uniforms.value.value = value?1:0;
};

Toggle.prototype.setFocus = function (value) {
  this.bgUniforms.focus.value = value?1:0;
};

Toggle.prototype.setLabel = function (text) {
  if (this.label) this.mesh.remove(this.label);

  var geometry = createText({
    text:  text,
    font:  font,
    align: 'left'
  });

  var material = textMaterial();
  this.label = new THREE.Mesh(geometry, material);
  this.label.scale.multiplyScalar(config.fontSize);
  this.label.position.x = this.height    / 2.0;
  this.label.position.y = 5 + this.height / 2.0;
  this.label.position.z = 0;

  this.mesh.add(this.label);
};

module.exports = Toggle;
