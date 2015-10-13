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


    $$.canvas2D = $('<canvas id="canvas2d" tabindex="0">')[0];
    document.body.appendChild($$.canvas2D);
    $$.canvas2DCtx = $$.canvas2D.getContext('2d');

    document.body.appendChild($$.renderer.domElement);

    window.displayObjectMap = function() { document.body.appendChild($$.rendererMap.domElement) };

    initCommonWidgets();
    textEditor.init();

    $('#log').remove();
    $('#spinner').remove();

    initTerminal();
    initUserInfo();
    $(document).unbind('keydown').bind('keydown', function (event) {
      if(event.keyCode == 8) {
        event.preventDefault();
        event.stopPropagation();
      };
    });

}

function initUserInfo() {
  $('body').append('<div id="userInfo"><div>Signed in as YC. <a class="tutorial" href="#">Tutorial</a></div></div>')
  $('body').append(require('tutorial')())

  if (localStorage.getItem('tutorial') == "1") $(".tutorial-box").hide();
  $(".tutorial-box").click(function(){ $(".tutorial-box").hide(); localStorage.setItem('tutorial', "1")});
  $(".tutorial").click(function(){ $(".tutorial-box").show() });
}
function initTerminal() {
  $('body').append('<div id="termContainer"><button id="termClose">Close</button><div id="term"></div></div>');
  $$.term = new Terminal({
    useStyle: true,
    rows: 20,
    cols: 40
  });

  $$.term.open($("#term")[0]);
  $$.term.write('\x1b[31mWelcome to NodeLab!\x1b[m\r\n');
  $("#termClose").click(function(){
    $('#termContainer').css({height: "0px"});
  });
}

function initCommonWidgets() {
  var colorId = 10;
  $$.currentConnection = new Connection(3, -1, colorId);
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


function createPendingNode(widgetId, expr, x, y) {
  var pos = new THREE.Vector2(x, y);
  var node = new GraphNode(-1, pos, nodeZOrderStart, widgetId);
  node.label(expr);
  node.setPending();
  $$.scene.add(node.mesh);
  $$.registry[widgetId] = node;
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

function createNodeSearcher(expression, nodeId, left, top) {
  var ns;
  if (features.node_searcher) {
    destroyNodeSearcher();
    ns = new NodeSearcher();
    $$.node_searcher = ns;
    $('body').append(ns.el);
    ns.init(nodeId);
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

function displayCurrentConnection(colorId, x0, y0, x1, y1) {
  $$.currentConnection.setPos(x0, y0, x1, y1);
  $$.currentConnection.show(colorId);
}

function removeCurrentConnection() {
  $$.currentConnection.hide();
}


function createConnection(widgetId, id, colorId) {
  var connection = new Connection(widgetId, id, colorId);
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

function removeWidget(widgetId) {
  var widget = $$.registry[widgetId];
  if (!widget) {
    console.error("RemoveWidget: widget " + widgetId + " does not exist.");
    return;
  }

  if(widget.destructor) {
    widget.destructor();
  }

  widget.mesh.parent.remove(widget.mesh);
  delete $$.registry[widgetId];
}

function writeToTerminal(str) {
  $('#termContainer').css({height: "300px"});
  $$.term.write(str)
}

var displayRejectedMessage = function () {
  $("canvas").remove();
  $("#htmlcanvas-pan").remove();
  $("#spinner").remove();
  $("#editor").remove();
  $("#rejected").show();
}

module.exports = {
  start:                    start,
  initializeGl:             initializeGl,
  render:                   render,
  moveToTopZ:               moveToTopZ,
  newNodeAt:                newNodeAt,
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
  removeWidget:             removeWidget,
  websocket:                $$.websocket,
  displayRejectedMessage:   displayRejectedMessage,
  createPendingNode:        createPendingNode,
  getNode:                  function(index) { return $$.nodes[index];    },
  getNodes:                 function()      { return _.values($$.nodes); },
  nodeSearcher:             function()      { return $$.node_searcher;   },
  shouldRender:             function()      { shouldRender = true;       },
  writeToTerminal:          writeToTerminal
};


