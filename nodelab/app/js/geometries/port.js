"use strict";

var triangleRatio = 1.5;
var size          = 9;

var distFromRim   = 3;
var nodeRadius    = 30.0;

var triangleHeight = size * Math.sqrt(3.0) / 2.0;
var halfHeight = triangleHeight / 2.0;
var dist  = nodeRadius + halfHeight + distFromRim;

var inPortGeometry, outPortGeometry;

var posL = new THREE.BufferAttribute(new Float32Array(9), 3);
posL.setXYZ(0, 0.0, 1.0, 1.0);
posL.setXYZ(1, 0.0, 1.0, 1.0);
posL.setXYZ(2, 1.0, 1.0, 0.0);

var posR = new THREE.BufferAttribute(new Float32Array(9), 3);
posR.setXYZ(0, 0.0, 1.0, 1.0);
posR.setXYZ(1, 1.0, 1.0, 0.0);
posR.setXYZ(2, 0.0, 1.0, 1.0);

var posB = new THREE.BufferAttribute(new Float32Array(9), 3);
posB.setXYZ(0, 1.0, 1.0, 0.0);
posB.setXYZ(1, 0.0, 1.0, 1.0);
posB.setXYZ(2, 0.0, 1.0, 1.0);


var pos = new Float32Array(9);
pos[0] = -halfHeight;
pos[1] = 0.0;
pos[2] = 0.0;
pos[3] = halfHeight;
pos[4] = size *  triangleRatio * 0.5;
pos[5] = 0.0;
pos[6] = halfHeight;
pos[7] = size * -triangleRatio * 0.5;
pos[8] = 0.0;

var position = new THREE.BufferAttribute(pos, 3);
var indicies = new THREE.BufferAttribute(new Uint16Array([0, 1, 2]), 1);

inPortGeometry = new THREE.BufferGeometry();

inPortGeometry.addAttribute('position', position);
inPortGeometry.setIndex(indicies);
inPortGeometry.addAttribute('posL', posL);
inPortGeometry.addAttribute('posR', posR);
inPortGeometry.addAttribute('posB', posB);

outPortGeometry = inPortGeometry.clone();
outPortGeometry.applyMatrix( new THREE.Matrix4().makeRotationZ(Math.PI) );

module.exports = {
  in: inPortGeometry,
  out: outPortGeometry,
  dist: dist,
};
