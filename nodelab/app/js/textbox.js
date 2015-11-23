"use strict";

var $$ = require('common');
var vs = require('shaders/slider.vert')();
var fs = require('shaders/slider.frag')();
var config = require('config');

var createText   = require('bmfont').render,
    font         = require("font/LatoBlack-sdf"),
    textMaterial = require('font/text_material').hud,
    layoutText   = require('bmfont').layout;

function TextBox(widgetId, width, height) {
  var _this = this;
  this.widgetId = widgetId;

  this.uniforms = {
    size:      { type: 'v2', value: new THREE.Vector2(width, height) },
    value:     { type: 'f',  value: 0.0 },
    focus:     { type: 'i',  value: 0   },
    objectId:  { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }
  };

  Object.keys($$.commonUniforms).forEach(function (k) {
    _this.uniforms[k] = $$.commonUniforms[k];
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
  this.bg.scale.x            = width;
  this.bg.scale.y            = height;

  this.mesh.add(this.bg);
}

TextBox.prototype.setFocus = function (value) {
  this.uniforms.focus.value = value?1:0;
};

TextBox.prototype.setLabel = function (text) {
  if (this.label) this.mesh.remove(this.label);

  var geometry = createText({
    text:  text,
    font:  font,
    align: 'left'
  });
  var material = textMaterial();
  this.label = new THREE.Mesh(geometry, material);
  this.label.scale.multiplyScalar(config.fontSize);
  this.label.position.x = this.uniforms.size.value.y / 2.0;
  this.label.position.y = 5 + this.uniforms.size.value.y / 2.0;
  this.label.position.z = 0;

  this.mesh.add(this.label);
};

TextBox.prototype.setValueLabel = function (text) {
  if (this.valueLabel) this.mesh.remove(this.valueLabel);

  var geometry = createText({
    text:  text,
    font:  font,
    align: 'left',
    width: (this.uniforms.size.value.x / 2.0) / (0.8 * config.fontSize),
    mode: "pre"
  });

  var material = textMaterial();
  this.valueLabel = new THREE.Mesh(geometry, material);
  this.valueLabel.scale.multiplyScalar(0.8 * config.fontSize);
  this.valueLabel.position.x = this.uniforms.size.value.x / 2.0;
  this.valueLabel.position.y = 5 + this.uniforms.size.value.y / 2.0;
  this.valueLabel.position.z = 0;

  this.mesh.add(this.valueLabel);
};

TextBox.prototype.startEditing = function(value) {
  if(this.input) this.input.remove();

  var input = $('<input type="text" class="widget"/>');
  var pos = this.mesh.localToWorld(new THREE.Vector3(this.bg.scale.x / 2.0, 0, 0));

  this.valueLabel.visible = false;

  this.input = input;

  input.css({left: pos.x, top: pos.y, width: this.bg.scale.x / 2.0 - 5});
  input.val(value);


  var saveChanges = function() {
    var evt = new Event('keydown');
    evt.keyCode = 13;
    evt.which   = 13;
    document.getElementById("canvas2d").dispatchEvent(evt);
  };

  var cancelChanges = function() {
    var evt = new Event('keydown');
    evt.keyCode = 27;
    evt.which   = 27;
    document.getElementById("canvas2d").dispatchEvent(evt);
  };

  input.on('keydown', function (ev) {
    if (ev.keyCode === 13) {
      saveChanges();
      ev.preventDefault();
    }
    if (ev.keyCode === 27) {
      cancelChanges();
      ev.preventDefault();
    }
    ev.stopPropagation();
  });

  input.on('blur', function (ev) {
    saveChanges();
    ev.stopPropagation();
  });


  $("#htmlcanvas").append(input);
  setTimeout(function (){ input.focus();}, 30);
};

TextBox.prototype.doneEditing = function() {
  if(this.input) this.input.remove();
  this.valueLabel.visible = true;
  this.input = null;
};

module.exports = TextBox;
