"use strict";

var $$             = require('common'),
    config         = require('config'),
    features       = require('features'),
    brunch         = require('brunch'),
    raycaster      = require('raycaster'),
    GraphNode      = require('node'),
    NodeSearcher   = require('node_searcher'),
    Connection     = require('connection'),
    SelectionBox   = require('selection_box'),
    websocket      = require('websocket'),
    textEditor     = require('text_editor'),
    connectionPen  = require('connection_pen');

console.info("Current version " + brunch.env + " " + brunch.git_commit);
console.info("Build at " + brunch.date);

$$.nodes             = {};
$$.currentConnection = null;
$$.selectionBox      = null;
$$.websocket         = websocket();


var nodeZOrderStep  = 0.00001;
var nodeZOrderStart = 0.00000;

var shouldRender = true;

function start() {
  $(document).ready(function(){
    $(document).bind('contextmenu', function() { return false; });
    require('env')();
  });
}

function initializeGl() {
    if(window.already_initialized) {
        location.reload();
    }

    window.already_initialized = true;
    $$.scene                 = new THREE.Scene();
    $$.sceneHUD              = new THREE.Scene();
    $$.camera                = new THREE.OrthographicCamera(-500, 500, -500, 500, 1, 1000);
    $$.cameraHUD             = new THREE.OrthographicCamera(-500, 500, -500, 500, 1, 1000);
    $$.camera.position.z     = 500;
    $$.cameraHUD.position.z  = 500;
    $$.renderer              = new THREE.WebGLRenderer({ antialias: false });
    $$.renderer.autoClear    = false;
    $$.rendererMap           = new THREE.WebGLRenderer({ antialias: false, preserveDrawingBuffer: true });
    $$.rendererMap.autoClear = false;
    $$.rendererMapCtx        = $$.rendererMap.getContext();


    $('body').append('<div id="htmlcanvas-pan"><div id="htmlcanvas"></div></div>');

    $$.htmlCanvasPan = $("#htmlcanvas-pan");
    $$.htmlCanvas    = $("#htmlcanvas");

    $$.renderer.setClearColor(config.backgroundColor, 1);
    $$.rendererMap.setClearColor(new THREE.Color("black"), 1);
    $($$.renderer.domElement).addClass('renderer');
    $($$.rendererMap.domElement).addClass('renderer').css({zIndex: 100});


    $$.canvas2D = $('<canvas id="canvas2d">')[0];
    document.body.appendChild($$.canvas2D);
    $$.canvas2DCtx = $$.canvas2D.getContext('2d');

    document.body.appendChild($$.renderer.domElement);

    // document.body.appendChild($$.rendererMap.domElement);

    initCommonWidgets();
    addVersionToHud();
    textEditor.init();

    $('#log').remove();
    $('#spinner').remove();

}

function addVersionToHud() {
  var createText   = require('bmfont').render;
  var font         = require("font/LatoBlack-sdf");
  var textMaterial = require('font/text_material').hud;

  var geom = createText({
    text: "Build at " + brunch.date,
    font: font,
    align: 'left'
  });

  var obj = new THREE.Mesh(geom, textMaterial);
  obj.scale.multiplyScalar(config.fontSize);
  obj.position.y = 20;
  obj.position.x = 500;

  $$.sceneHUD.add(obj);
}

function initCommonWidgets() {
  $$.currentConnection = new Connection(3, -1);
  $$.selectionBox      = new SelectionBox();

  $$.scene.add($$.currentConnection.mesh);
  $$.scene.add($$.selectionBox.mesh);
}

function render() {
  if(shouldRender) {
    $$.commonUniforms.objectMap.value = 0;
    $$.commonUniforms.antialias.value = 1;

    $$.renderer.clear();
    $$.renderer.render($$.scene, $$.camera);
    $$.renderer.clearDepth();
    $$.renderer.render($$.sceneHUD, $$.cameraHUD);

    raycaster.renderMap();
    shouldRender = false;
  }
  connectionPen.fadeCanvas();
  requestAnimationFrame(render);
}

function updateHtmCanvasPanPos(x, y, factor) {
  $$.htmlCanvasPan.css({left: x, top: y});
  $$.htmlCanvas.css({zoom: factor});
}

function updateScreenSize(width, height) {
  $$.renderer.setSize(width, height);
  $$.rendererMap.setSize(width, height);
  $$.canvas2D.width  = width;
  $$.canvas2D.height = height;
}

function updateCamera(factor, left, right, top, bottom) {
  $$.camFactor.value = factor;
  $$.camera.left     = left;
  $$.camera.right    = right;
  $$.camera.top      = top;
  $$.camera.bottom   = bottom;
}

function updateCameraHUD(left, right, top, bottom) {
  $$.cameraHUD.left     = left;
  $$.cameraHUD.right    = right;
  $$.cameraHUD.top      = top;
  $$.cameraHUD.bottom   = bottom;
}

function updateMouse(x, y) {
  _.values($$.nodes).forEach(function(node) {
    node.updateMouse(x, y);
  });
}

function newNodeAt(id, x, y, expr, widgetId) {
  var pos = new THREE.Vector2(x, y);
  var node = new GraphNode(id, pos, nodeZOrderStart + id * nodeZOrderStep, widgetId);
  $$.nodes[id] = node;
  node.label(expr);
  $$.scene.add(node.mesh);
  $$.registry[widgetId] = node;
}

function removeNode(i) {
  var node = $$.nodes[i];
  $$.scene.remove(node.mesh);
  delete $$.nodes[i];
  // delete $$.registry[65536 + i];
}

// -> HS
function moveToTopZ(nodeId) {
  var nodeToTop = $$.nodes[nodeId];
  var nodeToTopZ = nodeToTop.zPos();
  var maxZ = nodeToTop.zPos();
  _.values($$.nodes).forEach(function(node) {
    var nodeZ = node.zPos();
    if (nodeZ > nodeToTopZ) {
      node.zPos(nodeZ - nodeZOrderStep);
      if (nodeZ > maxZ) {
        maxZ = nodeZ;
      }
    }
  });
  nodeToTop.zPos(maxZ);
}

function createNodeSearcher(expression, left, top) {
  var ns;
  if (features.node_searcher) {
    destroyNodeSearcher();
    ns = new NodeSearcher();
    $$.node_searcher = ns;
    $('body').append(ns.el);
    ns.init();
    ns.el.css({left: left, top: top});
    if(expression)
      ns.setExpression(expression);
    return ns;
  }
}

function destroyNodeSearcher() {
  if ($$.node_searcher !== undefined) {
    $$.node_searcher.destroy();
  }
}

function displaySelectionBox(x0, y0, x1, y1) {
  $$.selectionBox.setPos(x0, y0, x1, y1);
  $$.selectionBox.show();
}

function hideSelectionBox() {
  $$.selectionBox.hide();
}

function displayCurrentConnection(x0, y0, x1, y1) {
  $$.currentConnection.setPos(x0, y0, x1, y1);
  $$.currentConnection.show();
}

function removeCurrentConnection() {
  $$.currentConnection.hide();
}


function createConnection(widgetId, id) {
  var connection = new Connection(widgetId, id);
  $$.scene.add(connection.mesh);
  $$.registry[widgetId] = connection;
}

function updateConnection(widgetId, visible, x0, y0, x1, y1) {
  $$.registry[widgetId].setPos(x0, y0, x1, y1);
  if(visible) {
    $$.registry[widgetId].show();
  } else {
    $$.registry[widgetId].hide();
  }
}

function removeConnection(widgetId) {
  var connection = $$.registry[widgetId];
  connection.mesh.parent.remove(connection.mesh);
  $$.registry[widgetId] = undefined;
}

var displayRejectedMessage = function () {
  $("canvas").remove();
  $("#htmlcanvas-pan").remove();
  $("#spinner").remove();
  $("#rejected").show();
}

module.exports = {
  start:                    start,
  initializeGl:             initializeGl,
  render:                   render,
  moveToTopZ:               moveToTopZ,
  newNodeAt:                newNodeAt,
  removeNode:               removeNode,
  updateHtmCanvasPanPos:    updateHtmCanvasPanPos,
  updateScreenSize:         updateScreenSize,
  updateCamera:             updateCamera,
  updateCameraHUD:          updateCameraHUD,
  updateMouse:              updateMouse,
  createNodeSearcher:       createNodeSearcher,
  destroyNodeSearcher:      destroyNodeSearcher,
  displaySelectionBox:      displaySelectionBox,
  hideSelectionBox:         hideSelectionBox,
  displayCurrentConnection: displayCurrentConnection,
  removeCurrentConnection:  removeCurrentConnection,

  createConnection:         createConnection,
  updateConnection:         updateConnection,
  removeConnection:         removeConnection,
  websocket:                $$.websocket,
  displayRejectedMessage:   displayRejectedMessage,
  getNode:                  function(index) { return $$.nodes[index];    },
  getNodes:                 function()      { return _.values($$.nodes); },
  nodeSearcher:             function()      { return $$.node_searcher;   },
  shouldRender:             function()      { shouldRender = true;       }
};


