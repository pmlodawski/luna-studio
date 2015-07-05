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
    setup_pan_and_zoom();
    document.addEventListener('keydown', onDocumentKeyDown, false );
    // createRandomNodes(1);

    // call -> HS
    render();

    window.ghcjs();
    if(features.node_searcher) {
      $$.node_searcher = new NodeSearcher();
      $('body').append($$.node_searcher.el);
    }
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
function setup_pan_and_zoom() {
  var dragMode = null;
  var mouse = new THREE.Vector2(0,0);
  var mouseStart = new THREE.Vector2(0,0);
  var RMB = 3, MMB = 2;
  $(document).bind('contextmenu', function(){return false;}); // prevent browser context menu

  $(document).mousedown(function(event) {
    switch(event.which) {
      case MMB: {
        event.preventDefault();
        dragMode = 'pan';
        event.stopPropagation();
        break;
      }
      case RMB: {
        dragMode = 'zoom';
        event.stopPropagation();
        break;
      }
      default: dragMode = null;
    }

    mouse.set(event.clientX, event.clientY);
    mouseStart.set(event.clientX - $$.halfScreen.x, - event.clientY + $$.halfScreen.y).divideScalar($$.camFactor.value);

  });

  $(document).mousemove(function(event) {
    var delta;
    switch(dragMode){
      case 'pan': {
        var position = utils.screenToWorkspace(event.clientX, event.clientY);
        delta = utils.screenToWorkspace(mouse.x, mouse.y).sub(position);
        $$.camPan.add(delta);
        reconfigure_camera();
        event.stopPropagation();
        break;
      }
      case 'zoom': {
        var deltaX = (event.clientX - mouse.x);
        var deltaY = -(event.clientY - mouse.y);
        var factor = (deltaX+deltaY)/512;
        delta = new THREE.Vector2(mouseStart.x, mouseStart.y).multiplyScalar(factor);
        if(updateCamFactor($$.camFactor.value * (1+factor))) {
          $$.camPan.add(delta);
          mouseStart.sub(delta);
        }
        reconfigure_camera();
        event.stopPropagation();
        break;
      }
    }
    mouse.set(event.clientX, event.clientY);
  });

  $(document).mouseup(function() {
    dragMode = null;
    // event.preventDefault();
  });
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
  reconfigure_camera();
}

// -> HS
function updateCamFactor(val) {
  $$.camFactor.value = Math.max($$.camFactorBounds[0], Math.min($$.camFactorBounds[1], val));
  return ($$.camFactor.value === val);
}

// -> HS
function reconfigure_camera() {
  $$.htmlCanvasPan.css({left: $$.halfScreen.x - $$.camPan.x * $$.camFactor.value, top: $$.halfScreen.y + $$.camPan.y * $$.camFactor.value});
  $$.htmlCanvas.css({zoom: $$.camFactor.value});

  $$.camera.left = - $$.halfScreen.x / $$.camFactor.value + $$.camPan.x;
  $$.camera.right = $$.halfScreen.x / $$.camFactor.value + $$.camPan.x;
  $$.camera.top = $$.halfScreen.y / $$.camFactor.value + $$.camPan.y;
  $$.camera.bottom = - $$.halfScreen.y / $$.camFactor.value + $$.camPan.y;

  $$.camera.updateProjectionMatrix();
}

function newNodeAt(i, x, y) {
    var vect = utils.screenToGl(x, y);
    console.log("adding new node " + i + " at " + vect.x + " " + vect.y);

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

// -> HS
function dragNode(nodeIndex, x, y) {
  var node;
  if (nodeIndex < 0)
    return;

  node = nodes[nodeIndex];
  var vec = utils.screenToWorkspace(x,y);
  node.moveTo(vec.x, vec.y);
}

module.exports = {
  render: render,
  dragNode: dragNode,
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
  start: start
};


// -> HS
function onDocumentKeyDown( event )
{
  switch(event.keyCode)
  {
    case 187: { // plus
      updateCamFactor($$.camFactor.value * 1.1);
      reconfigure_camera();
      break;
    }
    case 189: { // minus
      updateCamFactor($$.camFactor.value  / 1.1);
      reconfigure_camera();
      break;
    }
    case 37: { // left
      $$.camPan.x -= 10/$$.camFactor.value;
      reconfigure_camera();
      break;
    }
    case 38: { // top
      $$.camPan.y += 10/$$.camFactor.value;
      reconfigure_camera();
      break;
    }
    case 39: { // right
      $$.camPan.x += 10/$$.camFactor.value;
      reconfigure_camera();
      break;
    }
    case 40: { // bottom
      $$.camPan.y -= 10/$$.camFactor.value;
      reconfigure_camera();
      break;
    }
  }
}
