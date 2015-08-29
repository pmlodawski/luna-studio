"use strict";

var $$           = require('common');

function renderMap() {
  $$.commonUniforms.objectMap.value = 1;
  $$.commonUniforms.antialias.value = 0;
  $$.rendererMap.clear();
  $$.rendererMap.render($$.scene, $$.camera);
  $$.rendererMap.clearDepth();
  $$.rendererMap.render($$.sceneHUD, $$.cameraHUD);
}

function getMapPixelAt(x, y) {
  var buf = new Uint8Array(4);
  y = $$.rendererMap.domElement.height - y;
  $$.rendererMapCtx.readPixels(x, y, 1, 1, $$.rendererMapCtx.RGBA, $$.rendererMapCtx.UNSIGNED_BYTE, buf);
  return buf;
}

function getTopParent(w) {
  var p = w;
  while (p !== undefined) {
    w = p;
    p = w.parent;
  }
  return w;
}

function isWorkspace(id) {
  var widget = $$.registry[id];
  if (widget) {
    return (getTopParent(widget.mesh) !== $$.sceneHUD);
  } else {
    return false;
  }
}

function widgetMatrix(id) {
  var widget = $$.registry[id];
  var m = new THREE.Matrix4();
  if (widget) {
    m.getInverse(widget.mesh.matrixWorld);
    return m.elements;
  } else {
    return null;
  }
}

module.exports = {
  renderMap:     renderMap,
  getMapPixelAt: getMapPixelAt,
  widgetMatrix:  widgetMatrix,
  isWorkspace:   isWorkspace
};
