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
// var Port = require('triangle_port');

var insideColor     = new THREE.Color(0x1a1a1a);
var unselectedColor = new THREE.Color(0x3a3a3a);
var selectedColor   = new THREE.Color(0xb87410).multiplyScalar(0.8);
var focusedColor    = new THREE.Color(0xc85808).multiplyScalar(0.8);

var width  = 60;
var height = 60;

var nodeGeometry    = new THREE.PlaneBufferGeometry(width, height);

function Node(id, position, z) {
  var _this = this;

  this.id = id;
  this.position = position;

  this.labelText = ":" + id;
  this.inputPorts  = [];
  this.outputPorts = [];

  var _id = 1000 + id;

  this.uniforms = {
    selected:        { type: 'i',  value: 0 },
    mouseDist:       { type: 'f',  value: 100000 },
    insideColor:     { type: 'c',  value: insideColor },
    unselectedColor: { type: 'c',  value: unselectedColor },
    selectedColor:   { type: 'c',  value: selectedColor },
    focusedColor:    { type: 'c',  value: focusedColor },
    nodeSize:        { type: 'f',  value: 30.0 },
    objectId:        { type: 'v3', value: new THREE.Vector3((_id % 256) / 255.0, Math.floor(_id / 256) / 255.0, 0.0) }
  };

  Object.keys($$.commonUniforms).forEach(function(k) {
    _this.uniforms[k] = $$.commonUniforms[k];
  });

  this.mesh = new THREE.Mesh(
    nodeGeometry,
    new THREE.ShaderMaterial( {
      uniforms:       this.uniforms,
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
  this.zPos(z);
  this.updateMouse(position.x, position.y);

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

  // this.attributes.pos.needsUpdate = true;
  this.htmlContainer.css({left: vec.x, top: -vec.y});
};

Node.prototype.zPos = function(z) {
  if (z !== undefined) {
    this.mesh.position.z = z;
  }
  return this.mesh.position.z;
};

Node.prototype.updateMouse = function(x, y) {
  var xd = this.mesh.position.x - x;
  var yd = this.mesh.position.y - y;
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
  var p = new Port(id, angle, false, this.mesh.position.z);
  this.inputPorts.push(p);
  this.mesh.add(p.mesh);
  this.updateMouse(this.mesh.position.x, this.mesh.position.y);
};

Node.prototype.addOutputPort = function(id, angle) {
  var p = new Port(id, angle, true, this.mesh.position.z);
  this.outputPorts.push(p);
  this.mesh.add(p.mesh);
  this.updateMouse(this.mesh.position.x, this.mesh.position.y);
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

  var size = 150 / config.fontSize;

  var geometry = createText({
    text:  this.labelText,
    font:  font,
    width: size,
    align: 'center'
  });

  textMaterial.uniforms.width.value = size;
  this.labelObject = new THREE.Mesh(geometry, textMaterial);
  this.labelObject.rotation.x = 180 * Math.PI / 180;
  this.labelObject.scale.multiplyScalar(config.fontSize);
  this.labelObject.position.x = -75;
  this.labelObject.position.y = 42;
  this.labelObject.position.z = 0;

  this.mesh.add(this.labelObject);
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

module.exports = Node;
