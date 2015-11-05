"use strict";

var $$       = require('common');
var config   = require('config');
var features = require('features');

var createText   = require('bmfont').render;
var font         = require("font/LatoBlack-sdf");
var textMaterial = require('font/text_material').graph;

var vs = require('shaders/node.vert')();
var fs = require('shaders/node.frag')();

var evs = require('shaders/expandedNode.vert')();
var efs = require('shaders/expandedNode.frag')();

var insideColor     = new THREE.Color(0x1a1a1a);
var unselectedColor = new THREE.Color(0x3a3a3a);
var expandedColor   = new THREE.Color(0x202020);
var selectedColor   = new THREE.Color(0xb87410).multiplyScalar(0.8);
var focusedColor    = new THREE.Color(0xc85808).multiplyScalar(0.8);

var nodeGeometry    = new THREE.PlaneBufferGeometry(1.0, 1.0);
nodeGeometry.applyMatrix(new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0));


var expandedRadius  = 20.0;
var collapsedRadius = 30.0;

function Node(id, position, z, widgetId) {
  var _this = this;

  this.id = id;
  this.position = position;

  this.labelText = ":" + id;
  this.valueText = "";
  this.inputPorts  = [];
  this.outputPorts = [];

  this.uniforms = {
    selected:          { type: 'i',  value:                               0 },
    mouseDist:         { type: 'f',  value:                             0.0 },
    expanded:          { type: 'f',  value:                             0.0 },
    nodeSize:          { type: 'v2', value: new THREE.Vector2(2 * collapsedRadius, 2 * collapsedRadius) },
    radiusTop:         { type: 'f',  value:                 collapsedRadius },
    radiusBottom:      { type: 'f',  value:                 collapsedRadius },
    insideColor:       { type: 'c',  value:                     insideColor },
    unselectedColor:   { type: 'c',  value:                 unselectedColor },
    selectedColor:     { type: 'c',  value:                   selectedColor },
    focusedColor:      { type: 'c',  value:                    focusedColor },
    alpha:             { type: 'f',  value:                             1.0 },
    objectId:          { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }
  };

  this.expandedUniforms = {
    selected: this.uniforms.selected,
    expanded: this.uniforms.expanded,
    nodeSize:          { type: 'v2', value: new THREE.Vector2( 60.0,  60.0) },
    expandedColor:     { type: 'c',  value: expandedColor },
    objectId: this.uniforms.objectId,
    radiusTop:         { type: 'f',  value: collapsedRadius },
    radiusBottom:      { type: 'f',  value: collapsedRadius }
  };

  Object.keys($$.commonUniforms).forEach(function (k) {
    _this.uniforms[k] = $$.commonUniforms[k];
  });

  this.mesh = new THREE.Group();
  this.node = new THREE.Mesh(
    nodeGeometry,
    new THREE.ShaderMaterial( {
      uniforms:       this.uniforms,
      vertexShader:   vs,
      fragmentShader: fs,
      transparent:    true,
      blending:       THREE.NormalBlending,
      side:           THREE.DoubleSide
    })
  );
  this.node.position.set(-30, -30, 0);

  this.expandedNode = new THREE.Group();
  this.container    = this.expandedNode;

  this.expandedNodeBkg = new THREE.Mesh(
    nodeGeometry,
    new THREE.ShaderMaterial( {
      uniforms:       this.expandedUniforms,
      vertexShader:   evs,
      fragmentShader: efs,
      transparent:    true,
      blending:       THREE.NormalBlending,
      side:           THREE.DoubleSide
    })
  );
  this.expandedNode.position.set(-30, -30, 0);
  this.expandedNodeBkg.position.set(0, 0, -0.000005);

  this.mesh.add(this.node);
  this.mesh.add(this.expandedNode);
  this.expandedNode.add(this.expandedNodeBkg);
  this.mesh.userData.id = id;
  this.htmlContainer = document.createElement("div");
  $$.htmlCanvas.append(this.htmlContainer);

  this.htmlElements = {};
  this.moveTo(position.x, position.y);
  this.zPos(z);
  // this.updateMouse(position.x - 1000.0, position.y - 1000.0);

  this.collapsedNodeSize = new THREE.Vector2( 2 * collapsedRadius,  2 * collapsedRadius);
  this.expandedNodeSize  = new THREE.Vector2(200.0, 180.0);

  this.node.scale.x = this.collapsedNodeSize.x;
  this.node.scale.y = this.collapsedNodeSize.y;

  if (features.node_labels) this.updateLabel();
  this.updateValue();

  this.setExpandedState(0.0);
}

Node.prototype.setExpandedState = function (expanded) {
  this.expandedState = expanded;
  this.expandedNode.visible = (expanded > 0.0);

  var params = this.inputPorts.length;
  var nodeSize = new THREE.Vector2();
  nodeSize.x = (1.0 - expanded) * this.collapsedNodeSize.x + expanded * this.expandedNodeSize.x;
  nodeSize.y = (1.0 - expanded) * this.collapsedNodeSize.y + expanded * this.expandedNodeSize.y + params * 25 - 75;

  var radius = (1.0 - expanded) * collapsedRadius + expanded * expandedRadius;
  this.expandedUniforms.nodeSize.value.copy(nodeSize);
  this.expandedUniforms.radiusTop.value    = collapsedRadius;
  this.expandedUniforms.radiusBottom.value = radius;
  this.expandedNodeBkg.scale.x = nodeSize.x;
  this.expandedNodeBkg.scale.y = nodeSize.y;
  this.repositionPlot();
};

Node.prototype.setPending = function () {
  this.uniforms.alpha.value = 0.2;
};

Node.prototype.setExpandedStateBool = function(expanded) {
  if (expanded) {
    this.setExpandedState(1.0);
  } else {
    this.setExpandedState(0.0);
  }
};

Node.prototype.selected = function (val) {
  if (val !== undefined) {
    this.uniforms.selected.value = val;
    if (features.label_editor) {
      if (val === 2) {
        this.showLabelEditor();
      }
    }
  }
  return this.uniforms.selected.value;
};

Node.prototype.moveTo = function (a, b) {
  var vec = new THREE.Vector2(a, b);

  this.position.x = vec.x;
  this.position.y = vec.y;

  this.mesh.position.x = vec.x;
  this.mesh.position.y = vec.y;

  $(this.htmlContainer).css({left: vec.x, top: vec.y});
};

Node.prototype.zPos = function (z) {
  if (z !== undefined) {
    this.mesh.position.z = z;
  }
  return this.mesh.position.z;
};

// Node.prototype.updateMouse = function (x, y) {
//   var xd = (this.mesh.position.x - this.mesh.scale.x / 2.0) - x;
//   var yd = (this.mesh.position.y - this.mesh.scale.y / 2.0) - y;
//   var mouseDist = Math.sqrt(xd * xd + yd * yd);
//   this.uniforms.mouseDist.value = mouseDist;
//   this.inputPorts.forEach(function (port) {
//     port.updateMouseDist(mouseDist);
//   });
//   this.outputPorts.forEach(function (port) {
//     port.updateMouseDist(mouseDist);
//   });
// };
//
// Node.prototype.addInputPort = function (oid, id, colorId, angle) {
//   this.addPort(this.inputPorts, false, oid, id, colorId, angle);
// };
//
// Node.prototype.addOutputPort = function (oid, id, colorId, angle) {
//   this.addPort(this.outputPorts, true, oid, id, colorId, angle);
// };
//
// Node.prototype.addPort = function (ports, out, oid, id, colorId, angle) {
//   var p = new Port(id, oid, colorId, angle, out);
//   ports.push(p);
//   this.mesh.add(p.mesh);
//   this.updateMouse(this.mesh.position.x - 1000.0, this.mesh.position.y - 1000.0);
//   $$.registry[oid] = p;
// };
//
// Node.prototype.findInputPort = function (id) {
//   return _.find(this.inputPorts, function (port) { return port.id === id; });
// };
//
// Node.prototype.findOutputPort = function (id) {
//   return _.find(this.outputPorts, function (port) { return port.id === id; });
// };
//
// Node.prototype.setInputPortAngle = function (id, angle) {
//   this.findInputPort(id).setAngle(angle);
// };
//
// Node.prototype.setOutputPortAngle = function (id, angle) {
//   this.findOutputPort(id).setAngle(angle);
// };
//
// Node.prototype.setInputPortColor = function (id, r, g, b, a) {
//   this.findInputPort(id).setColor(new THREE.Vector4(r, g, b, a));
// };
//
// Node.prototype.setOutputPortColor = function (id, r, g, b, a) {
//   this.findOutputPort(id).setColor(new THREE.Vector4(r, g, b, a));
// };

Node.prototype.label = function (text) {
  if (text !== undefined) {
    this.labelText = text;
    this.updateLabel();
  }
  return this.labelText;
};

Node.prototype.updateLabel = function () {
  if (this.labelObject) this.mesh.remove(this.labelObject);

  var size = 150 / config.fontSize;

  var geometry = createText({
    text:  this.labelText,
    font:  font,
    width: size,
    align: 'center'
  });
  var material = textMaterial();
  material.uniforms.width.value = size;
  material.uniforms.opacity = this.uniforms.alpha;
  this.labelObject = new THREE.Mesh(geometry, material);
  this.labelObject.scale.multiplyScalar(config.fontSize);
  this.labelObject.position.x = -45 - 30;
  this.labelObject.position.y = -12 - 30;
  this.labelObject.position.z =   0;

  this.mesh.add(this.labelObject);
};

Node.prototype.setValue = function (text) {
  this.valueText = text;
  this.updateValue();
  return this.valueText;
};

Node.prototype.updateValue = function () {
  if (this.valueObject) this.mesh.remove(this.valueObject);

  var size = 150 / config.fontSize;

  var geometry = createText({
    text:  this.valueText,
    font:  font,
    width: size,
    align: 'center'
  });
  var material = textMaterial();
  material.uniforms.width.value = size;
  this.valueObject = new THREE.Mesh(geometry, material);
  this.valueObject.scale.multiplyScalar(config.fontSize);
  this.valueObject.position.x = -45 - 30;
  this.valueObject.position.y = 12 + 30 + 10;
  this.valueObject.position.z = 0;

  this.mesh.add(this.valueObject);
};

Node.prototype.showLabelEditor = function () {
  if (this.htmlElements.labelEditor) return;
  var editor = $('<input/>').css({left: -50, top: -52, width: 100, textAlign: 'center'});
  editor.val(this.labelText);
  this.htmlElements.labelEditor = editor;
  this.htmlContainer.append(editor);

  this.mesh.remove(this.labelObject);
  this.labelObject = null;
  var _this = this;
  setTimeout(function (){
    editor.focus();
    editor.blur(function (){
        _this.label(_this.hideLabelEditor());
    });
  }, 50);
};

Node.prototype.repositionPlot = function () {
  var topPosition = this.expandedState > 0.0 ? 130 : 60;
  var topCss = "top: " + topPosition + "px;";
  d3.select(this.htmlContainer)
    .select("svg")
    .attr("style", "position: absolute; left: -30px;" + topCss);
};

Node.prototype.displayVector = function (values) {
  $(this.htmlContainer).empty();

  var svg = d3.select(this.htmlContainer)
      .append("svg")
      .attr("width", 250)
      .attr("height", 180);
  var data = values.map(function (val, ix) {
    return {
      Index: ix,
      Value: val
    };
  });
  var myChart = new dimple.chart(svg, data);
  myChart.addCategoryAxis("x", "Index");
  this.repositionPlot();
  myChart.addMeasureAxis("y", "Value");
  myChart.addSeries(null, dimple.plot.bubble);
  // myChart.addLegend(180, 10, 360, 20, "right");
  myChart.draw();
};

Node.prototype.destructor = function (){
  this.htmlContainer.parentNode.removeChild(this.htmlContainer);
};

Node.prototype.hideLabelEditor = function () {
  var editor = this.htmlElements.labelEditor;
  var value = editor[0].value;
  console.log("Entered: " + value);
  editor.off();
  editor.remove();
  this.htmlElements.labelEditor = null;
  return value;
};

module.exports = Node;
