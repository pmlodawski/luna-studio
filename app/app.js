"use strict";

var $            = require('jquery'),
    _            = require('underscore'),
    THREE        = require('three'),
    FunctionNode = require('./function_node').FunctionNode,
    $$           = require('./common'),
    config       = require('./config'),
    utils        = require('./utils'),
    features     = require('./features'),
    NodeSearcher = require('./node_searcher');

var nodes = {};    // -> HS
$$.nodes = nodes;

var zOrderDiv = 10000.0;
var currentMazZ = 0.0;
var maxZ = (Math.pow(2, 31) - 1) / zOrderDiv; // -> HS

// var maxNodeNr = 0;

// export to HTML
function start() {
  $(document).ready(function(){
    THREE = require('three');
    initialize_gl();

    // -> HS
    // setupPanAndZoom();
    // createRandomNodes(1);
    $(document).bind('contextmenu', function(){return false;}); // prevent browser context menu


    // call -> HS
    render();

    window.ghcjs();
    // if(features.node_searcher) {
    //   $$.node_searcher = new NodeSearcher();
    //   $('body').append($$.node_searcher.el);
    // }
  });
}

function render() {
  $$.renderer.render($$.scene, $$.camera);
  requestAnimationFrame(render);
}

function initialize_gl() {
    $$.scene = new THREE.Scene();

    $$.camera = new THREE.OrthographicCamera(-500, 500, -500, 500, 1, 1000); // camera position will be updated in reconfigure_camera
    $$.renderer = new THREE.WebGLRenderer({ antialias: true });
    $$.renderer.setClearColor( config.backgroundColor, 1 ); // set background color

    $($$.renderer.domElement).addClass('renderer');
    document.body.appendChild( $$.renderer.domElement );

    $$.camera.position.z = 500;

    window.addEventListener('resize', setWindowSize);
    setWindowSize();
}


// -> HS
function setWindowSize(){
  var w = window.innerWidth;
  var h = window.innerHeight;

  $$.screenSize.x = w;
  $$.screenSize.y = h;
  $$.halfScreen.x = w / 2.0;
  $$.halfScreen.y = h / 2.0;
  $$.renderer.setSize( w, h );
  reconfigureCamera();
}

function updateHtmCanvasPanPos(x, y, factor) {
  $$.htmlCanvasPan.css({left: x, top: y});
  $$.htmlCanvas.css({zoom: factor});
}


function updateCamera(factor, camPanX, camPanY, left, right, top, bottom) {
  // console.log("fact " + factor + " " + left + " " + right + " " + top + " " + bottom);
  $$.camFactor.value = factor;
  $$.camPan.x        = camPanX;
  $$.camPan.y        = camPanY;
  $$.camera.left     = left;
  $$.camera.right    = right;
  $$.camera.top      = top;
  $$.camera.bottom   = bottom;
}

// -> HS
function reconfigureCamera() {
  var htmlX = $$.halfScreen.x - $$.camPan.x * $$.camFactor.value;
  var htmlY = $$.halfScreen.y + $$.camPan.y * $$.camFactor.value;

  $$.camera.left   = -$$.halfScreen.x / $$.camFactor.value + $$.camPan.x;
  $$.camera.right  =  $$.halfScreen.x / $$.camFactor.value + $$.camPan.x;
  $$.camera.top    =  $$.halfScreen.y / $$.camFactor.value + $$.camPan.y;
  $$.camera.bottom = -$$.halfScreen.y / $$.camFactor.value + $$.camPan.y;

  updateHtmCanvasPanPos(htmlX, htmlY, $$.camFactor.value);
  $$.camera.updateProjectionMatrix();
}

function newNodeAt(i, x, y) {
    // var vect = utils.screenToGl(x, y);
    var vect = utils.workspaceToGl(x, y);
    // console.log("adding new node " + i + " at " + vect.x + " " + vect.y);

    var node = new FunctionNode(i, vect);
    nodes[i] = node;
    $$.scene.add(node.mesh);
}

function removeNode(i) {
  var node = nodes[i];
  $$.scene.remove(node.mesh);
  delete nodes[i];
}


// -> HS
function isSelectedNodeIdOnOffset(nodeId, xy) {
  var d_squared = xy[0] * xy[0] + xy[1] * xy[1];
  return d_squared <= 900;
}


// -> HS
function assignZs() {
  var sortedNodes = _.values(nodes);
  sortedNodes.sort(function(nodeA, nodeB) {
      return nodeA.position.z - nodeB.position.z;
  } );
  for (var i = 0; i < sortedNodes.length; i++)
      sortedNodes[i].zPos(i / zOrderDiv);

  currentMazZ = sortedNodes[sortedNodes.length - 1].zPos();
}

// -> HS
function moveToTopZ(nodeId) {
  var node = nodes[nodeId];
  node.zPos(currentMazZ + 1.0 / zOrderDiv);
  currentMazZ = node.zPos();
  if (currentMazZ > maxZ)
      assignZs();
}

// -> HS (redefined)
function getNodeAt(x, y) {
  var emptyNode = [-1, 0, 0, 0];
  var nodeIds = getNodeIdsOnPosition(x, y);

  if (nodeIds.length === 0) return emptyNode;

  for (var i = 0; i < nodeIds.length; i++) {
    var nodeId = nodeIds[i];
    var node = nodes[nodeId];
    var offset = getOffsetForNode(nodeId, x, y);
    if (isSelectedNodeIdOnOffset(nodeId, offset)) {
      var xy = utils.workspaceToScreen(node.position.x, node.position.y);
      return [nodeId, node.selected(), xy.x, xy.y];
    }
  }
  return emptyNode;
}

// -> HS ? (maybe not necessary)
function getNodeIdsOnPosition(x, y) {
    var rayCaster = new THREE.Raycaster();
    rayCaster.setFromCamera(utils.screenToNormalizedGl(x, y), $$.camera);
    var intersects = rayCaster.intersectObjects($$.scene.children);

    var ids = intersects.map(function(intersect){return intersect.object.userData.id;});
    return ids;
}

// -> HS ?
function getOffsetForNode(nodeIndex, x, y) {
    if (nodeIndex < 0)
        return [0, 0];

    var vec = utils.screenToWorkspace(x,y);
    var node = nodes[nodeIndex];

    return node.position.clone().sub(vec).toArray();
}

function createNodeSearcher(expression, left, top) {
  var ns;
  if(features.node_searcher) {
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
  if($$.node_searcher !== undefined) {
    $$.node_searcher.destroy();
  }
}

module.exports = {
  render: render,
  moveToTopZ: moveToTopZ,
  getNodeAt: getNodeAt,  // remove
  getNode: function(index) {
    return $$.nodes[index];
  },
  getNodes: function() {
    return _.values($$.nodes);
  },
  newNodeAt: newNodeAt,
  removeNode: removeNode,
  updateHtmCanvasPanPos: updateHtmCanvasPanPos,
  updateCamera: updateCamera,
  start: start,
  createNodeSearcher: createNodeSearcher,
  destroyNodeSearcher: destroyNodeSearcher,
  nodeSearcher: function() { return $$.node_searcher; }
};


