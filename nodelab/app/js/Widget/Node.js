"use strict";

var $$       = require('common');

var vs = require('shaders/sdf.vert')();
var fs = require('shaders/node.frag')();

var unselectedColor = new THREE.Color(0x3a3a3a);
var selectedColor   = new THREE.Color(0xb87410).multiplyScalar(0.8);
var errorColor      = new THREE.Color(0xec2f02);

var nodeGeometry    = new THREE.PlaneBufferGeometry(1.0, 1.0);
nodeGeometry.applyMatrix(new THREE.Matrix4().makeTranslation(0.5, 0.5, 0.0));


var nodeSize = 100.0;

function Node(position, z, widgetId) {
  var _this = this;

  this.position = position;

  this.uniforms = {
    selected:          { type: 'i',  value:                               0 },
    mouseDist:         { type: 'f',  value:                             0.0 },
    expanded:          { type: 'f',  value:                             0.0 },
    size:              { type: 'v2', value:   new THREE.Vector2(nodeSize, nodeSize) },
    unselectedColor:   { type: 'c',  value:                 unselectedColor },
    selectedColor:     { type: 'c',  value:                   selectedColor },
    errorColor:        { type: 'c',  value:                      errorColor },
    alpha:             { type: 'f',  value:                             1.0 },
    error:             { type: 'i',  value:                               0 },
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
  this.node.position.set(-nodeSize/2, -nodeSize/2, 0);

  this.expandedNode = new THREE.Group();
  this.container    = this.mesh;
  this.mesh.add(this.node);

  this.node.scale.x = nodeSize;
  this.node.scale.y = nodeSize;

  this.mesh.position.x = position.x;
  this.mesh.position.y = position.y;

}

Node.prototype.setPending = function () {
  this.uniforms.alpha.value = 0.2;
};

Node.prototype.setSelected = function (val) {
  this.uniforms.selected.value = val?1:0;
};
Node.prototype.setError = function (val) {
  this.uniforms.error.value = val?1:0;
};


Node.prototype.setZPos = function (z) {
  this.mesh.position.z = z;
};

Node.prototype.destructor = function () {};

Node.prototype.redrawTextures = function () {};

module.exports = Node;
