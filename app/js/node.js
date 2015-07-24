"use strict";

var $$       = require('common');
var config   = require('config');
var features = require('features');

var createText   = require('bmfont').render;
var font         = require("font/LatoBlack-sdf");
var textMaterial = require('font/text_material').graph;

var vs = require('shaders/node.vert')();
var fs = require('shaders/node.frag')();

var Port = require('port');

var insideColor     = new THREE.Vector3(0.1, 0.1, 0.1);
var unselectedColor = new THREE.Vector3(0.2, 0.2, 0.2);
var overColor       = new THREE.Vector3(0.3, 0.3, 0.3);
var selectedColor   = new THREE.Vector3(0.85, 0.55, 0.1);
var focusedColor    = new THREE.Vector3(0.90, 0.40, 0.05);

function Node(id, position) {
  var width  = 60;
  var height = 60;
  var _this = this;

  this.id = id;
  this.position = position;
  this.labelText = ":" + id;
  this.inputPorts  = [];
  this.outputPorts = [];

  this.attributes = {
    pos: {
      type: 'v2',
      value: [
        position,
        position,
        position,
        position
      ]
    }
  };

  this.uniforms = {
    selected:        { type:  'i', value: 0 },
    mouseDist:       { type:  'f', value: 100000 },
    insideColor:     { type: 'v3', value: insideColor },
    unselectedColor: { type: 'v3', value: unselectedColor },
    overColor:       { type: 'v3', value: overColor },
    selectedColor:   { type: 'v3', value: selectedColor },
    focusedColor:    { type: 'v3', value: focusedColor }
  };

  Object.keys($$.commonUniforms).forEach(function(k) {
    _this.uniforms[k] = $$.commonUniforms[k];
  });

  this.mesh = new THREE.Mesh(
    new THREE.PlaneGeometry(width, height),
    new THREE.ShaderMaterial( {
      uniforms:       this.uniforms,
      attributes:     this.attributes,
      vertexShader:   vs,
      fragmentShader: fs,
      transparent:    true,
      blending:       THREE.NormalBlending
    })
  );
  this.mesh.userData.id = id;
  this.htmlContainer = $('<div/>');
  $$.htmlCanvas.append(this.htmlContainer);

  this.htmlElements = {};
  this.moveTo(position.x, position.y);

  if (features.node_labels) this.updateLabel();
}

Node.prototype.selected = function(val) {
  if (val !== undefined) {
    this.uniforms.selected.value = val;
    if(features.label_editor) {
      if(val === 2) {
        this.showLabelEditor();
      }
    }
  }
  return this.uniforms.selected.value;
};

Node.prototype.moveTo = function(a, b) {
  var vec = new THREE.Vector2(a, b);

  this.position.x = vec.x;
  this.position.y = vec.y;

  this.mesh.position.x = vec.x;
  this.mesh.position.y = vec.y;

  this.attributes.pos.needsUpdate = true;
  this.htmlContainer.css({left: vec.x, top: -vec.y});
};

Node.prototype.zPos = function(z) {
  if (z !== undefined)
    this.mesh.position.z = z;
  return this.mesh.position.z;
};

Node.prototype.updateMouse = function(x, y) {
  var xd = this.position.x - x;
  var yd = this.position.y - y;
  var mouseDist = Math.sqrt(xd * xd + yd * yd);
  this.uniforms.mouseDist.value = mouseDist;
  this.inputPorts.forEach(function(port) {
    port.updateMouseDist(mouseDist);
  });
  this.outputPorts.forEach(function(port) {
    port.updateMouseDist(mouseDist);
  });
};

Node.prototype.addInputPort = function(id, angle) {
  var p = new Port(id, angle, false);
  this.inputPorts.push(p);
  this.mesh.add(p.mesh);
};

Node.prototype.addOutputPort = function(id, angle) {
  var p = new Port(id, angle, true);
  this.outputPorts.push(p);
  this.mesh.add(p.mesh);
};

Node.prototype.findInputPort = function(id) {
  return _.find(this.inputPorts, function(port) { return port.id === id; });
};

Node.prototype.findOutputPort = function(id) {
  return _.find(this.outputPorts, function(port) { return port.id === id; });
};

Node.prototype.setInputPortAngle = function(id, angle) {
  this.findInputPort(id).setAngle(angle);
};

Node.prototype.setOutputPortAngle = function(id, angle) {
  this.findOutputPort(id).setAngle(angle);
};

Node.prototype.setInputPortColor = function(id, r, g, b, a) {
  this.findInputPort(id).setColor(new THREE.Vector4(r, g, b, a));
};

Node.prototype.setOutputPortColor = function(id, r, g, b, a) {
  this.findOutputPort(id).setColor(new THREE.Vector4(r, g, b, a));
};

Node.prototype.label = function(text) {
  if (text !== undefined) {
    this.labelText = text;
    this.updateLabel();
  }
  return this.labelText;
};

Node.prototype.updateLabel = function() {
  if (this.labelObject) this.mesh.remove(this.labelObject);

  var geom = createText({
    text: this.labelText,
    font: font,
    width: 150/config.fontSize,
    align: 'center'
  });

  var obj = new THREE.Mesh(geom, textMaterial);
  obj.rotation.x = 180 * Math.PI/180;
  obj.scale.multiplyScalar(config.fontSize);
  obj.position.x = -75;
  obj.position.z = 0.0001;
  obj.position.y = 35;

  this.labelObject = obj;
  this.mesh.add(obj);
};

Node.prototype.showLabelEditor = function() {
  if (this.htmlElements.labelEditor) return;
  var editor = $('<input/>').css({left: -50, top: -52, width: 100, textAlign: 'center'});
  editor.val(this.labelText);
  this.htmlElements.labelEditor = editor;
  this.htmlContainer.append(editor);

  this.mesh.remove(this.labelObject);
  this.labelObject = null;
  var _this = this;
  setTimeout(function(){
    editor.focus();
    editor.blur(function(){
        _this.label(_this.hideLabelEditor());
    });
  }, 50);
};

Node.prototype.renderExamplePlot = function() {
  var svg = d3.select(this.htmlContainer[0])
      .append("svg")
      .attr("width", 400)
      .attr("height", 250);
  var i;
  var data = [];
  for (i = 0; i < 12; i++) {
    data[2*i]   = { "Month": i, "Unit Sales": Math.random() * 30, "Channel": "direct" };
    data[2*i+1] = { "Month": i, "Unit Sales": Math.random() * 30, "Channel": "web"    };
  }

  var myChart = new dimple.chart(svg, data);
  var x = myChart.addCategoryAxis("x", "Month");
  x.addOrderRule("Date");
  myChart.addMeasureAxis("y", "Unit Sales");
  myChart.addSeries("Channel", dimple.plot.bubble);
  myChart.addLegend(180, 10, 360, 20, "right");
  myChart.draw();
};

Node.prototype.hideLabelEditor = function() {
  var editor = this.htmlElements.labelEditor;
  var value = editor[0].value;
  console.log("Entered: " + value);
  editor.off();
  editor.remove();
  this.htmlElements.labelEditor = null;
  return value;
};

module.exports.Node = Node;
