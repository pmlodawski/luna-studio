"use strict";

var FunctionNode = require('function_node').FunctionNode,
    $$           = require('common'),
    config       = require('config'),
    features     = require('features'),
    brunch       = require('brunch'),
    breadcrumb   = require('breadcrumb'),
    NodeSearcher = require('node_searcher'),
    Connection   = require('connection'),
    SelectionBox = require('selection_box'),
    Button       = require('button');

console.info("Current version " + brunch.env + " " + brunch.git_commit);
console.info("Build at " + brunch.date);

$$.nodes             = {};
$$.connections       = {};
$$.currentConnection = null;
$$.selectionBox      = null;

var zOrderDiv = 10000.0;
var currentMazZ = 0.0;
var maxZ = (Math.pow(2, 31) - 1) / zOrderDiv; // -> HS

// export to HTML
function start() {
  $(document).ready(function(){
    $(document).bind('contextmenu', function() { return false; });
    require('env')();
  });
}

function initializeGl() {
    if(window.already_initialized) {
        alert("Alredy initialized… it shouldn't happen");
    }

    window.already_initialized = true;
    $$.scene                = new THREE.Scene();
    $$.sceneHUD             = new THREE.Scene();
    $$.camera               = new THREE.OrthographicCamera(-500, 500, -500, 500, 1, 1000);
    $$.cameraHUD            = new THREE.OrthographicCamera(-500, 500, -500, 500, 1, 1000);
    $$.camera.position.z    = 500;
    $$.cameraHUD.position.z = 500;
    $$.renderer             = new THREE.WebGLRenderer({ antialias: false });
    $$.renderer.autoClear   = false;

    $('body').append('<div id="htmlcanvas-pan"><div id="htmlcanvas"></div></div>');

    $$.renderer.setClearColor(config.backgroundColor, 1);
    initCommonWidgets();
    addVersionToHud();
    $($$.renderer.domElement).addClass('renderer');

    $$.renderScene = new THREE.RenderPass($$.scene, $$.camera);
    $$.effectFXAA  = new THREE.ShaderPass(THREE.FXAAShader);
    $$.effectFXAA.renderToScreen = true;

    $$.composer = new THREE.EffectComposer($$.renderer);
    $$.composer.addPass($$.renderScene);
    $$.composer.addPass($$.effectFXAA);


    document.body.appendChild($$.renderer.domElement);

    breadcrumb.initialize();
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
  // obj.rotation.x = 180 * Math.PI/180;
  obj.scale.multiplyScalar(config.fontSize);
  obj.position.y = 20;

  $$.sceneHUD.add(obj);
}

function initCommonWidgets() {
  $$.currentConnection = new Connection(0);
  $$.selectionBox      = new SelectionBox();

  $$.scene.add($$.currentConnection.mesh);
  $$.scene.add($$.selectionBox.mesh);
}

function render() {
  $$.renderer.clear();
  // $$.renderer.render($$.scene, $$.camera);

  $$.composer.render();

  $$.renderer.clearDepth();
  $$.renderer.render($$.sceneHUD, $$.cameraHUD);
  requestAnimationFrame(render);
}

function updateHtmCanvasPanPos(x, y, factor) {
  $$.htmlCanvasPan.css({left: x, top: y});
  $$.htmlCanvas.css({zoom: factor});
}

function updateScreenSize(width, height) {
  $$.screenSize.x = width;
  $$.screenSize.y = height;
  $$.renderer.setSize(width, height);

  var dpr = 1;
  if (window.devicePixelRatio !== undefined) {
    dpr = window.devicePixelRatio;
  }
  $$.effectFXAA.uniforms.resolution.value.set(1 / (window.innerWidth * dpr), 1 / (window.innerHeight * dpr));
  $$.composer.setSize(window.innerWidth * dpr, window.innerHeight * dpr);
}

function updateCamera(factor, camPanX, camPanY, left, right, top, bottom) {
  $$.camFactor.value = factor;
  $$.camPan.x        = camPanX;
  $$.camPan.y        = camPanY;
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

function newNodeAt(i, x, y, expr) {
  var vect = new THREE.Vector2(x, y);
  var node = new FunctionNode(i, vect);
  $$.nodes[i] = node;
  node.label(expr);
  $$.scene.add(node.mesh);
}

function removeNode(i) {
  var node = $$.nodes[i];
  $$.scene.remove(node.mesh);
  delete $$.nodes[i];
}

// -> HS
function assignZs() {
  var sortedNodes = _.values($$.nodes);
  sortedNodes.sort(function(nodeA, nodeB) {
      return nodeA.position.z - nodeB.position.z;
  });
  for (var i = 0; i < sortedNodes.length; i++)
      sortedNodes[i].zPos(i / zOrderDiv);
  currentMazZ = sortedNodes[sortedNodes.length - 1].zPos();
}

// -> HS
function moveToTopZ(nodeId) {
  var node = $$.nodes[nodeId];
  node.zPos(currentMazZ + 1.0 / zOrderDiv);
  currentMazZ = node.zPos();
  if (currentMazZ > maxZ)
      assignZs();
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

module.exports = {
  initializeGl:             initializeGl,
  render:                   render,
  moveToTopZ:               moveToTopZ,
  newNodeAt:                newNodeAt,
  removeNode:               removeNode,
  updateHtmCanvasPanPos:    updateHtmCanvasPanPos,
  updateScreenSize:         updateScreenSize,
  updateCamera:             updateCamera,
  updateCameraHUD:          updateCameraHUD,
  start:                    start,
  createNodeSearcher:       createNodeSearcher,
  destroyNodeSearcher:      destroyNodeSearcher,
  displaySelectionBox:      displaySelectionBox,
  hideSelectionBox:         hideSelectionBox,
  displayCurrentConnection: displayCurrentConnection,
  removeCurrentConnection:  removeCurrentConnection,
  getNode:                  function(index) { return $$.nodes[index]; },
  getNodes:                 function()      { return _.values($$.nodes); },
  nodeSearcher:             function()      { return $$.node_searcher; }
};


