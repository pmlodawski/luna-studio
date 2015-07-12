/* globals dimple: false, d3: false */

"use strict";

var THREE = require('three');
var $ = require('jquery');
var $$ = require('./common');
var features = require('./features');

var createText = require('three-bmfont-text');
var font = require("./font/LatoBlack-sdf");

var fs = require('./shaders/node.frag');
var vs = require('./shaders/node.vert');

var textMaterial = new THREE.ShaderMaterial(require('./shaders/font')({
  map: THREE.ImageUtils.loadTexture('font/LatoBlack-sdf.png'),
  smooth: 1/12,
  side: THREE.DoubleSide,
  transparent: true,
  color: 'rgb(230, 230, 230)'
}));

function FunctionNode(id, position){
  var width  = 60;
  var height = 60;
  var _this = this;

  this.id = id;
  this.position = position;
  this.labelText = ":" + id;

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
      selected: { type: "i", value: 0 }
  };

  Object.keys($$.commonUniforms).forEach(function(k) { // copy common uniforms
    _this.uniforms[k] = $$.commonUniforms[k];
  });

  this.mesh = new THREE.Mesh(
      new THREE.PlaneGeometry(width, height),
      new THREE.ShaderMaterial( {
        uniforms: this.uniforms,
        attributes: this.attributes,
        vertexShader:   vs(),
        fragmentShader: fs(),
        transparent: true,
        blending: THREE.NormalBlending
      })
  );
  this.mesh.userData.id = id;
  this.htmlContainer = $('<div/>');
  $$.htmlCanvas.append(this.htmlContainer);

  this.htmlElements = {};
  this.moveTo(position.x, position.y);

  if(features.node_labels) this.updateLabel();
}

FunctionNode.prototype.selected = function(val) {
  if(val !== undefined) {
    // console.log("selection state: " + val);
    this.uniforms.selected.value = val;
    if(features.label_editor) {
      if(val === 2) {
        this.showLabelEditor();
      }
    }
  }
  return this.uniforms.selected.value;
};

FunctionNode.prototype.moveTo = function(a, b) {
  var vec = new THREE.Vector2(a, b);

  this.position.x = vec.x;
  this.position.y = vec.y;

  this.mesh.position.x = vec.x;
  this.mesh.position.y = vec.y;

  this.attributes.pos.needsUpdate = true;
  this.htmlContainer.css({left: vec.x, top: -vec.y});
};

FunctionNode.prototype.zPos = function(z) {
  if(z !== undefined)
    this.mesh.position.z = z;
  return this.mesh.position.z;
};


FunctionNode.prototype.label = function(text) {
  if(text !== undefined) {
    this.labelText = text;
    this.updateLabel();
  }
  return this.labelText;
};

FunctionNode.prototype.updateLabel = function() {
  if(this.labelObject) this.mesh.remove(this.labelObject);

  var geom = createText({
    text: this.labelText,
    font: font,
    width: 100/0.35,
    align: 'center'
  });

  var obj = new THREE.Mesh(geom, textMaterial);
  obj.rotation.x = 180 * Math.PI/180;
  obj.scale.multiplyScalar(0.35);
  obj.position.x = -50;
  obj.position.z = 0.0001;
  obj.position.y = 35;

  this.labelObject = obj;
  this.mesh.add(obj);
};

FunctionNode.prototype.showLabelEditor = function() {
  if(this.htmlElements.labelEditor) return;
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

FunctionNode.prototype.renderExamplePlot = function() {

  var svg = d3.select(this.htmlContainer[0])
      .append("svg")
      .attr("width", 400)
      .attr("height", 250);
  var i;
  var data = [];
  for(i = 0; i < 12; i++) {
    data[2*i] = {"Month": i, "Unit Sales": Math.random()*30, "Channel": "direct"};
    data[2*i+1] = {"Month": i, "Unit Sales": Math.random()*30, "Channel": "web"};
  }

  var myChart = new dimple.chart(svg, data);
  var x = myChart.addCategoryAxis("x", "Month");
  x.addOrderRule("Date");
  myChart.addMeasureAxis("y", "Unit Sales");
  myChart.addSeries("Channel", dimple.plot.bubble);
  myChart.addLegend(180, 10, 360, 20, "right");
  myChart.draw();
};

FunctionNode.prototype.hideLabelEditor = function() {
  var editor = this.htmlElements.labelEditor;
  var value = editor[0].value;
  console.log("Entered: " + value);
  editor.off();
  editor.remove();
  this.htmlElements.labelEditor = null;
  return value;
};

module.exports.FunctionNode = FunctionNode;
