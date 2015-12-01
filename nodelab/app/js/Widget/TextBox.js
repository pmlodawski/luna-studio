"use strict";

var $$ = require('common');
var config = require('config');

var createText   = require('bmfont').render,
    font         = require("font/LatoBlack-sdf"),
    textMaterial = require('font/text_material').hud;

function TextBox(widgetId, width, height) {
  var _this = this;
  this.widgetId = widgetId;
  this.width  = width;
  this.height = height;
  this.mesh = new THREE.Group();
}

TextBox.prototype.setValueLabel = function (text) {
  if (this.valueLabel) this.mesh.remove(this.valueLabel);

  var geometry = createText({
    text:  text,
    font:  font,
    align: 'left',
    width: this.width / (0.8 * config.fontSize),
    mode: "pre"
  });

  var material = textMaterial();
  this.valueLabel = new THREE.Mesh(geometry, material);
  this.valueLabel.scale.multiplyScalar(0.8 * config.fontSize);
  this.valueLabel.position.y = 5 + this.height / 2.0;

  this.mesh.add(this.valueLabel);
};

TextBox.prototype.startEditing = function(value) {
  if(this.input) this.input.remove();

  var input = $('<input type="text" class="widget"/>');
  var pos = this.mesh.localToWorld(new THREE.Vector3(0, 0, 0));

  this.valueLabel.visible = false;

  this.input = input;

  input.css({left: pos.x, top: pos.y, width: this.width});
  input.val(value);


  var saveChanges = function() {
    var evt     = new Event('keydown');
    evt.keyCode = 13;
    evt.which   = 13;
    document.getElementById("canvas2d").dispatchEvent(evt);
  };

  var cancelChanges = function() {
    var evt     = new Event('keydown');
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
