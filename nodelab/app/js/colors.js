"use strict";

var colors = [hslToVec(0,0.0,0.5)];

var start = 90.6 / (2 * Math.PI),
    steps = 16,
    delta = 1.0 / steps;

function hslToVec(h,s,l) {
  var c = new THREE.Color().setHSL(h,s,l);
  return new THREE.Vector4(c.r, c.g, c.b, 1.0);
}


for(var i = 0; i < steps; ++i) {
  var hue = start + delta * i;
  colors.push(hslToVec(hue*2*Math.PI, 0.6, 0.5));
}
module.exports = colors;
