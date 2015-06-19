var THREE = require('three');
var common = require('./common');

var createText = require('three-bmfont-text');
var font = require("./font/LatoBlack-sdf");

var fs = require('./shaders/node.frag');
var vs = require('./shaders/node.vert');

var utils = require('./utils');

var textMaterial = new THREE.ShaderMaterial(require('./shaders/font')({
  map: THREE.ImageUtils.loadTexture('font/LatoBlack-sdf.png'),
  smooth: 1/16,
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
  
  Object.keys(common.commonUniforms).forEach(function(k) { // copy common uniforms
    _this.uniforms[k] = common.commonUniforms[k];
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
  this.moveTo(position.x, position.y);
  
  this.updateLabel();
  // console.log(this)
  // var text = renderText("# " + i)
  // text.position.z = 0.00009
  // text.position.x = -20
  // nodes[i].add(text)
}

FunctionNode.prototype.selected = function(val) {
  if(val !== undefined)
    this.uniforms.selected.value = val;
  return this.uniforms.selected.value;
};

FunctionNode.prototype.moveTo = function(a, b) {
  var vec = utils.ensureVector(a, b);
  
  this.position.x = vec.x;
  this.position.y = vec.y;
  
  this.mesh.position.x = vec.x;
  this.mesh.position.y = vec.y;
  
  this.attributes.pos.needsUpdate = true;
};

FunctionNode.prototype.zPos = function(z) {
  if(z !== undefined) 
    this.mesh.position.z = z;
  return this.mesh.position.z;
};

FunctionNode.prototype.updateLabel = function() {  
  if(this.label) this.mesh.remove(this.label);
  var geom = createText({
    text: this.labelText,
    font: font,
    width: 700,
  });
  
  this.label = new THREE.Mesh(geom, textMaterial);
  this.label.rotation.x = 180 * Math.PI/180;
  this.label.scale.multiplyScalar(0.5);
  this.label.position.z = 0.0001;
  this.label.position.x = -20;
  this.label.position.y = -5;
  this.mesh.add(this.label);
};

module.exports.FunctionNode = FunctionNode;