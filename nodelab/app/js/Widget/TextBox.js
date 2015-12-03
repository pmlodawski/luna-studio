"use strict";

var config = require('config');

var createText   = require('bmfont').render,
    font         = require("font/LatoBlack-sdf"),
    textMaterial = require('font/text_material').hud,
    layoutText   = require('bmfont').layout;

function TextBox(widgetId, width, height) {
  this.widgetId  = widgetId;
  this.width     = width;
  this.height    = height;
  this.mesh      = new THREE.Group();
  this.value     = "";
  this.alignment = "Left";
}

TextBox.prototype.setValueLabel = function (text) {
  if (this.valueLabel) this.mesh.remove(this.valueLabel);

  this.value = text;

  var layout = {
    text:  text,
    font:  font,
    align: this.alignment,
    mode: "pre"
  };

  var width = layoutText(layout).width * 0.8 * config.fontSize;

  var geometry = createText(layout);

  var material = textMaterial();
  this.valueLabel = new THREE.Mesh(geometry, material);
  this.valueLabel.scale.multiplyScalar(0.8 * config.fontSize);
  this.valueLabel.position.y = 5 + this.height / 2.0;

  switch(this.alignment) {
    case 'Left':
      this.valueLabel.position.x = 0;
      break;
    case 'Right':
      this.valueLabel.position.x = this.width - width;
      break;
    case 'Center':
      this.valueLabel.position.x = (this.width - width) / 2.0;
      break;
    default:
      console.error("Invalid text alignment");
  }

  this.mesh.add(this.valueLabel);
};

TextBox.prototype.setAlignment = function(align) {
  this.alignment = align;
  this.setValueLabel(this.value);
};

TextBox.prototype.startEditing = function(value) {
  if(this.input) this.input.remove();

  var input = $('<input type="text" class="widget"/>');
  var pos = this.mesh.localToWorld(new THREE.Vector3(0, 0, 0));

  this.valueLabel.visible = false;
  this.input = input;

  input.css({left: pos.x, top: pos.y, width: this.width, textAlign: this.alignment});
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
