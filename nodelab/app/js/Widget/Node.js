"use strict";

var $$       = require('common');
var features = require('features');

var vs = require('shaders/sdf.vert')();
var fs = require('shaders/node.frag')();

var insideColor     = new THREE.Color(0x1a1a1a);
var unselectedColor = new THREE.Color(0x3a3a3a);
var selectedColor   = new THREE.Color(0xb87410).multiplyScalar(0.8);
var focusedColor    = new THREE.Color(0xc85808).multiplyScalar(0.8);

var nodeGeometry    = new THREE.PlaneBufferGeometry(1.0, 1.0);
nodeGeometry.applyMatrix(new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0));


var collapsedRadius = 30.0;

function Node(position, z, widgetId) {
  var _this = this;

  this.position = position;

  this.uniforms = {
    selected:          { type: 'i',  value:                               0 },
    mouseDist:         { type: 'f',  value:                             0.0 },
    expanded:          { type: 'f',  value:                             0.0 },
    size:              { type: 'v2', value:   new THREE.Vector2(60.0, 60.0) },
    radiusTop:         { type: 'f',  value:                 collapsedRadius },
    radiusBottom:      { type: 'f',  value:                 collapsedRadius },
    insideColor:       { type: 'c',  value:                     insideColor },
    unselectedColor:   { type: 'c',  value:                 unselectedColor },
    selectedColor:     { type: 'c',  value:                   selectedColor },
    focusedColor:      { type: 'c',  value:                    focusedColor },
    alpha:             { type: 'f',  value:                             1.0 },
    objectId:          { type: 'v3', value: new THREE.Vector3((widgetId % 256) / 255.0, Math.floor(Math.floor(widgetId % 65536) / 256) / 255.0, Math.floor(widgetId / 65536) / 255.0) }
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
      side:           THREE.DoubleSide,
      derivatives:    true
    })
  );
  this.node.position.set(-30, -30, 0);

  this.expandedNode = new THREE.Group();
  this.container    = this.mesh;
  this.mesh.add(this.node);

  this.collapsedNodeSize = new THREE.Vector2(60.0, 60.0);

  this.node.scale.x = this.collapsedNodeSize.x;
  this.node.scale.y = this.collapsedNodeSize.y;

  this.mesh.position.x = position.x;
  this.mesh.position.y = position.y;

}

Node.prototype.setPending = function () {
  this.uniforms.alpha.value = 0.2;
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


Node.prototype.setZPos = function (z) {
  this.mesh.position.z = z;
};

Node.prototype.destructor = function (){
  this.htmlContainer.parentNode.removeChild(this.htmlContainer);
};

Node.prototype.redrawTextures = function() {};

module.exports = Node;
