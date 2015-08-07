"use strict";

var $$           = require('common'),
    config       = require('config');

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
  var ctx = $$.rendererMap.getContext();
  y = $$.screenSize.y - y;
  ctx.readPixels(x, y, 1, 1, ctx.RGBA, ctx.UNSIGNED_BYTE, buf);
  return buf;
}

function getTopParent(w) {
  var p = w;
  while (p !== undefined) {
    w = p;
    p = w.parent;
  }
  return w;
};

function screenToWorkspace(vec) {
  var x, y;
  x = vec.x;
  y = vec.y;

  x =  x - $$.screenSize.x / 2.0;
  y = -y + $$.screenSize.y / 2.0;

  x = x / $$.camFactor.value + $$.camPan.x;
  y = y / $$.camFactor.value + $$.camPan.y;
  return new THREE.Vector2(x, y);
}

function toWidgetLocal(id, x, y) {
  var widget = $$.registry[id];
  var pos = new THREE.Vector2(x, y);
  var vec;
  if (widget) {
    if (getTopParent(widget.mesh) !== $$.sceneHUD) {
      pos = screenToWorkspace(pos);
    }
    vec = widget.mesh.worldToLocal(new THREE.Vector3(pos.x, pos.y, 0.0));
    return [vec.x, vec.y];
  } else {
    return null;
  }
}

module.exports = {
  renderMap:     renderMap,
  getMapPixelAt: getMapPixelAt,
  toWidgetLocal: toWidgetLocal
}