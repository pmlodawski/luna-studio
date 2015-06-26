var THREE = require('three');
var $$ = require('./common');

function ensureVector(a, b) {
  var x, y;
  if(a instanceof THREE.Vector2) {
    x = a.x;
    y = a.y;
  } else {
    x = a;
    y = b;
  }
  return new THREE.Vector2(x, y);  
}

function screenToGl(a, b) {
  var vec = ensureVector(a, b);
  return new THREE.Vector2(vec.x - $$.screenSize.x / 2, - vec.y + $$.screenSize.y / 2);
}

function screenToNormalizedGl(a, b) {
  var vec = ensureVector(a, b);
  return new THREE.Vector2(vec.x / $$.screenSize.x * 2 - 1, -(vec.y / $$.screenSize.y) * 2 + 1);
} 

function glToWorkspace(a, b) {
  var vec = ensureVector(a, b);

  return new THREE.Vector2(vec.x / $$.camFactor.value + $$.camPan.x, vec.y / $$.camFactor.value + $$.camPan.y);  
}

function screenToWorkspace(a, b) {
  return glToWorkspace(screenToGl(a,b));
} 

function workspaceToScreen(a, b) {
  var vec = ensureVector(a, b);

  return new THREE.Vector2((vec.x  - $$.camPan.x) * $$.camFactor.value + $$.halfScreen.x , (- vec.y + $$.camPan.y) * $$.camFactor.value + $$.halfScreen.y );
}

module.exports = {
  ensureVector: ensureVector,
  screenToGl: screenToGl,
  screenToNormalizedGl: screenToNormalizedGl,
  screenToWorkspace: screenToWorkspace,
  glToWorkspace: glToWorkspace,
  workspaceToScreen: workspaceToScreen
};