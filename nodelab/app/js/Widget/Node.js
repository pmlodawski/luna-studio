"use strict";

var $$       = require('common');
var features = require('features');

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
  this.container    = this.mesh; //this.expandedNode;

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
  // this.updateMouse(position.x - 1000.0, position.y - 1000.0);

  this.collapsedNodeSize = new THREE.Vector2( 2 * collapsedRadius,  2 * collapsedRadius);
  this.expandedNodeSize  = new THREE.Vector2(200.0, 180.0);

  this.node.scale.x = this.collapsedNodeSize.x;
  this.node.scale.y = this.collapsedNodeSize.y;

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

Node.prototype.setZPos = function (z) {
  this.mesh.position.z = z;
};

Node.prototype.destructor = function (){
  this.htmlContainer.parentNode.removeChild(this.htmlContainer);
};

Node.prototype.redrawTextures = function() {}

module.exports = Node;
