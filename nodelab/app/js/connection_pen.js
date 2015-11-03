"use strict";

var $$        = require('common'),
    raycaster = require('raycaster');

var iterations = 0;
var tempCanvas = document.createElement('canvas');
var type = false;

function clearCanvas() {
  $$.canvas2DCtx.clearRect(0, 0, $$.canvas2D.width, $$.canvas2D.height);
}

function beginPath(x, y, _type) {
  type = _type;
  $$.canvas2DCtx.beginPath();
  $$.canvas2DCtx.moveTo(x, y);
  raycaster.cacheMap();
  iterations = -1;
  $$.canvas2DCtx.strokeStyle = (type ? "#00ff00" : "#ff0000");
  $$.canvas2DCtx.lineCap = 'round';
  $$.canvas2DCtx.globalAlpha = 1.0;
}

function drawSegment(x, y) {
  $$.canvas2DCtx.lineTo(x, y);
  $$.canvas2DCtx.stroke();
}

function endPath() {
  iterations = 40;
}

function fadeCanvas() {
  if (iterations > 0) {

    tempCanvas.width  = $$.canvas2D.width;
    tempCanvas.height = $$.canvas2D.height;
    tempCanvas.getContext('2d').drawImage($$.canvas2D, 0, 0);
    clearCanvas();
    $$.canvas2DCtx.globalAlpha = 0.90;
    $$.canvas2DCtx.drawImage(tempCanvas, 0, 0, tempCanvas.width, tempCanvas.height, 0, 0, $$.canvas2D.width, $$.canvas2D.height);

    iterations -= 1;
  } else {
    if (iterations === 0) {
      clearCanvas();
      iterations = -1;
    }
  }
}

function colEq(a, b) {
  return (a[0] === b[0]) && (a[1] === b[1]) && (a[2] === b[2]) && (a[3] === b[3]);
}

function getWidgetsBetween(x1, y1, x2, y2) {
  var points = [];
  var apnd = function (px, py) {
    var idAt = raycaster.getMapPixelAtCached(px, py);
    if (points.length === 0 || !colEq(points[points.length-1], idAt))
      points.push(idAt);
  };

  var dx = x2 - x1;
  var dy = y2 - y1;

  // Determine how steep the line is
  var is_steep = Math.abs(dy) > Math.abs(dx);

  // Rotate line
  if (is_steep) {
    y1 = [x1, x1 = y1][0];
    y2 = [x2, x2 = y2][0];
  }

  // Swap start and end points if necessary and store swap state
  var swapped = false;
  if (x1 > x2) {
    x2 = [x1, x1 = x2][0];
    y2 = [y1, y1 = y2][0];
    swapped = true;
  }

  // Recalculate differentials
  dx = x2 - x1;
  dy = y2 - y1;

  // Calculate error
  var error = dx / 2;
  var ystep = (y1 < y2) ? 1 : -1;

  // Iterate over bounding box generating points between start and end
  var y = y1;
  for (var x = x1; x < x2 + 1; x += 1) {
    var coord = (is_steep) ? [y, x] : [x, y];
    apnd(coord[0], coord[1]);
    error -= Math.abs(dy);
    if (error < 0) {
      y += ystep;
      error += dx;
    }
  }

  // Reverse the list if the coordinates were swapped
  if (swapped) {
    points.reverse();
  }

  return points;
}

function colorToId(col) {
  return col[0] + 256 * col[1] + 65535 * col[2];
}

function requestWidgetsBetween(x1, y1, x2, y2) {
  var widgets = getWidgetsBetween(x1, y1, x2, y2).map(colorToId);

  module.exports.callback(widgets);
}



module.exports = {
  clearCanvas: clearCanvas,
  beginPath: beginPath,
  drawSegment: drawSegment,
  fadeCanvas: fadeCanvas,
  getWidgetsBetween: getWidgetsBetween,
  requestWidgetsBetween: requestWidgetsBetween,
  endPath: endPath,
  callback: function() {
    console.error("ConnectionPen: Callback was not registered.");
  }
};
