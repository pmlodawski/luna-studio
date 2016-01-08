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

// function getMapPixelAt(x, y) {
//   var buf = new Uint8Array(4);
//   y = $$.rendererMap.domElement.height - y;
//   $$.rendererMapCtx.readPixels(x, y, 1, 1, $$.rendererMapCtx.RGBA, $$.rendererMapCtx.UNSIGNED_BYTE, buf);
//   return buf;
// }



var cachedMap = null;
var cachedWidth = 0;
var cachedHeight = 0;

function cacheMap() {
  var width  = $$.rendererMap.domElement.width,
      height = $$.rendererMap.domElement.height;
  cachedWidth  = width;
  cachedHeight = height;
  cachedMap = new Uint8Array(4 * width * height);
  $$.rendererMapCtx.readPixels(0, 0, width, height, $$.rendererMapCtx.RGBA, $$.rendererMapCtx.UNSIGNED_BYTE, cachedMap);
}

function getMapPixelAtCached(x, y) {
  var offset = 4 * (cachedWidth * (cachedHeight - y - 1) + x);
  return [ cachedMap[offset + 0]
         , cachedMap[offset + 1]
         , cachedMap[offset + 2]
         , cachedMap[offset + 3]
         ];
}

var getMapPixelAt = getMapPixelAtCached;

var getObjectsInRect = function(x, y, w, h) {
  var last = -1;
  var out = [];

  for(var j = y; j < y + h; j++) {
    for(var i = x; i < x + w; i++) {
      var offset = 4 * (cachedWidth * (cachedHeight - j - 1) + i);
      var id = cachedMap[offset + 0] + 256 * cachedMap[offset + 1] + 256 * 256 * cachedMap[offset + 2];
      if(id !== last) {
        out.push(id);
        last = id;
      }
    }
  }

  return _.uniq(out);
};

function getTopParent(w) {
  var p = w;
  while (p !== undefined && p !== null) {
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
  cacheMap: cacheMap,
  getCachedMap: function() {return cachedMap; },
  getMapPixelAt: getMapPixelAt,
  getMapPixelAtCached: getMapPixelAtCached,
  getObjectsInRect: getObjectsInRect,
  widgetMatrix:  widgetMatrix,
  isWorkspace:   isWorkspace
};
