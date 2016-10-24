"use strict";

var $$           = require('common');


function renderMap() {
  $$.renderer.setClearColor(new THREE.Color("black"), 1);
  $$.commonUniforms.objectMap.value = 1;
  $$.commonUniforms.antialias.value = 0;
  $$.renderer.render($$.scene, $$.camera, $$.rendererMap, true);
  $$.renderer.clearTarget($$.rendererMap, false, true, false);
  $$.renderer.render($$.sceneHUD, $$.cameraHUD, $$.rendererMap);
}

var cachedMap    = null;
var cachedWidth  = 0;
var cachedHeight = 0;

function cacheMap() {
  var width  = $$.renderer.getSize().width  * $$.renderer.getPixelRatio(),
      height = $$.renderer.getSize().height * $$.renderer.getPixelRatio();
  if (cachedWidth !== width || cachedHeight !== height)
      cachedMap = new Uint8Array(4 * width * height);
  cachedWidth  = width;
  cachedHeight = height;
  $$.renderer.readRenderTargetPixels($$.rendererMap, 0,0, width, height, cachedMap);
}

function getMapPixelAtCached(x, y) {
  x = x * $$.renderer.getPixelRatio();
  y = y * $$.renderer.getPixelRatio();
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

  x = x * $$.renderer.getPixelRatio();
  y = y * $$.renderer.getPixelRatio();
  w = w * $$.renderer.getPixelRatio();
  h = h * $$.renderer.getPixelRatio();

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
  renderMap:           renderMap,
  cacheMap:            cacheMap,
  getCachedMap:        function() {return cachedMap; },
  getMapPixelAt:       getMapPixelAt,
  getMapPixelAtCached: getMapPixelAtCached,
  getObjectsInRect:    getObjectsInRect,
  widgetMatrix:        widgetMatrix,
  isWorkspace:         isWorkspace
};
