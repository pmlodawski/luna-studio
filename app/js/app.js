"use strict";

var FunctionNode = require('function_node').FunctionNode,
    $$           = require('common'),
    config       = require('config'),
    features     = require('features'),
    NodeSearcher = require('node_searcher');

console.log("Current version {!env!} {!git_commit!}");
console.log("Build at {!date!}");

var nodes = {};
$$.nodes = nodes;

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
    $$.scene = new THREE.Scene();
    $$.camera = new THREE.OrthographicCamera(-500, 500, -500, 500, 1, 1000);
    $$.camera.position.z = 500;
    $$.renderer = new THREE.WebGLRenderer({ antialias: true });
    $$.renderer.setClearColor(config.backgroundColor, 1);
    $($$.renderer.domElement).addClass('renderer');
    document.body.appendChild($$.renderer.domElement);
}

function render() {
  $$.renderer.render($$.scene, $$.camera);
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

function newNodeAt(i, x, y) {
  var vect = new THREE.Vector2(x, y);
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
function assignZs() {
  var sortedNodes = _.values(nodes);
  sortedNodes.sort(function(nodeA, nodeB) {
      return nodeA.position.z - nodeB.position.z;
  });
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

module.exports = {
  initializeGl: initializeGl,
  render: render,
  moveToTopZ: moveToTopZ,
  getNode: function(index) {
    return $$.nodes[index];
  },
  getNodes: function() {
    return _.values($$.nodes);
  },
  newNodeAt: newNodeAt,
  removeNode: removeNode,
  updateHtmCanvasPanPos: updateHtmCanvasPanPos,
  updateScreenSize: updateScreenSize,
  updateCamera: updateCamera,
  start: start,
  createNodeSearcher: createNodeSearcher,
  destroyNodeSearcher: destroyNodeSearcher,
  nodeSearcher: function() { return $$.node_searcher; }
};


