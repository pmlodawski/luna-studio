"use strict";

// var THREE = require('three');
var $$ = require('common');

function screenToGl(a, b) {
  var vec = new THREE.Vector2(a, b);
  return new THREE.Vector2(vec.x - $$.screenSize.x / 2, - vec.y + $$.screenSize.y / 2);
}

function screenToNormalizedGl(a, b) {
  var vec = new THREE.Vector2(a, b);
  return new THREE.Vector2(vec.x / $$.screenSize.x * 2 - 1, -(vec.y / $$.screenSize.y) * 2 + 1);
}

function glToWorkspace(a, b) {
  var vec = new THREE.Vector2(a, b);
  return new THREE.Vector2(vec.x / $$.camFactor.value + $$.camPan.x, vec.y / $$.camFactor.value + $$.camPan.y);
}

function screenToWorkspace(a, b) {
  var vec = screenToGl(a,b);
  return glToWorkspace(vec.x, vec.y);
}

function workspaceToScreen(a, b) {
  var vec = new THREE.Vector2(a, b);
  return new THREE.Vector2((vec.x  - $$.camPan.x) * $$.camFactor.value + $$.halfScreen.x , (- vec.y + $$.camPan.y) * $$.camFactor.value + $$.halfScreen.y );
}

module.exports = {
  screenToGl: screenToGl,
  screenToNormalizedGl: screenToNormalizedGl,
  screenToWorkspace: screenToWorkspace,
  glToWorkspace: glToWorkspace,
  workspaceToScreen: workspaceToScreen
};
