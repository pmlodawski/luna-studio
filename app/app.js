"use strict";

var $ = require('jquery');
var _ = require('underscore');
var THREE = require('three');
var FunctionNode = require('./function_node').FunctionNode;
var $$ = require('./common');
var config = require('./config');
var utils = require('./utils');

var nodes = {};    // array of nodes with nodes
$$.nodes = nodes;

var zOrderDiv = 10000.0;
var currentMazZ = 0.0;
var maxZ = (Math.pow(2, 31) - 1) / zOrderDiv;


// export to HTML
function start() {
  $(document).ready(function(){
    window.ghcjs();
    THREE = require('three');
    initialize_gl();
    setup_pan_and_zoom();
    
    // setup_pan_and_drag()
    document.addEventListener('keydown', onDocumentKeyDown, false );
    createRandomNodes(40);
    render();
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
        break;
      }
      case RMB: dragMode = 'zoom'; break;
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
        delta = utils.screenToWorkspace(mouse).sub(position);
        $$.camPan.add(delta);
        reconfigure_camera();
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

function updateCamFactor(val) {
  $$.camFactor.value = Math.max($$.camFactorBounds[0], Math.min($$.camFactorBounds[1], val));
  return ($$.camFactor.value === val);
}

function reconfigure_camera() {
  $$.htmlCanvasPan.css({left: $$.halfScreen.x - $$.camPan.x * $$.camFactor.value, top: $$.halfScreen.y + $$.camPan.y * $$.camFactor.value});
  $$.htmlCanvas.css({zoom: $$.camFactor.value});

  $$.camera.left = - $$.halfScreen.x / $$.camFactor.value + $$.camPan.x;
  $$.camera.right = $$.halfScreen.x / $$.camFactor.value + $$.camPan.x;
  $$.camera.top = $$.halfScreen.y / $$.camFactor.value + $$.camPan.y;
  $$.camera.bottom = - $$.halfScreen.y / $$.camFactor.value + $$.camPan.y;

  $$.camera.updateProjectionMatrix();
}

function createRandomNodes(number) {
    var posX;
    var posY;

    // nodes
    var width  = 60;     // nodes' width
    var height = 60;    // nodes' height
    for (var i = 0; i < number; i++)
    {
        posX = (Math.random() - 0.5) * (window.innerWidth - width);  // possible X coords of planes
        posY = (Math.random() - 0.5) * (window.innerHeight - height);  // possible Y coords of planes

        var node = new FunctionNode(i, new THREE.Vector2(posX, posY));
        nodes[i] = node;
        $$.scene.add(node.mesh);
    }
    assignZs();
}





function isSelectedNodeIdOnOffset(nodeId, xy) {
  var d_squared = xy[0] * xy[0] + xy[1] * xy[1];
  return d_squared <= 900;
}

// export
// function isNodeSelected(nodeId) {
//     return uniforms[nodeId].selected.value == 1;
// }

// export
function setNodeFocused(nodeId) {
  unfocusNode();
  nodes[nodeId].selected(2);
  moveToTopZ(nodeId);
}

// export
function setNodeSelected(nodeId) {
  nodes[nodeId].selected(1);
  moveToTopZ(nodeId);
}

// export
function setNodeUnselected(nodeId) {
  nodes[nodeId].selected(0);
}

// export
function unselectAllNodes() {
  _.each(nodes, function(node){
    node.selected(0);
  });
}

// export
function unfocusNode() {
  _.each(nodes, function(node){
      if (node.selected() === 2) node.selected(1);
  });
}

// export
// function toggleNodeSelection(nodeId) {
//     console.log("toggleNodeSelection " + nodeId)
//     uniforms[nodeId].selected.value = 1 - uniforms[nodeId].selected.value
// }

function assignZs() {
  var sortedNodes = _.values(nodes);
  sortedNodes.sort(function(nodeA, nodeB) {
      return nodeA.position.z - nodeB.position.z;
  } );
  for (var i = 0; i < sortedNodes.length; i++)
      sortedNodes[i].zPos(i / zOrderDiv);

  currentMazZ = sortedNodes[sortedNodes.length - 1].zPos();
}

function moveToTopZ(nodeId) {
  var node = nodes[nodeId];
  node.zPos(currentMazZ + 1.0 / zOrderDiv);
  currentMazZ = node.zPos();
  if (currentMazZ > maxZ)
      assignZs();
}

// export
// function addNodeIdWithOffsetOnPosition(x, y) {
//     var emptyNode = [-1, 0, 0];
    // var nodeIds = getNodeIdsOnPosition(x, y);
    // // unselectAll();
    // if (nodeIds.length === 0) return emptyNode;
    // for (var i = 0; i < nodeIds.length; i++) {
    //     var nodeId = nodeIds[i];
    //     var xy = getOffsetForNode(nodeId, x, y);
    //     if (isSelectedNodeIdOnPosition(nodeId, xy)) {
    //         rearrangeNodesZOrder(nodes[nodeId]);
    //         selectNode(nodeId);
    //         return [nodeId, xy[0], xy[1]];
    //     }
    // }
//     return emptyNode;
// }

// export
function getNodeAt(x, y) {
  var emptyNode = [-1, 0, 0, 0];
  var nodeIds = getNodeIdsOnPosition(x, y);

  if (nodeIds.length === 0) return emptyNode;
      
  for (var i = 0; i < nodeIds.length; i++) {
    var nodeId = nodeIds[i];
    var node = nodes[nodeId];
    var offset = getOffsetForNode(nodeId, x, y);
    if (isSelectedNodeIdOnOffset(nodeId, offset)) {
      var xy = utils.workspaceToScreen(node.position);
      return [nodeId, node.selected(), xy.x, xy.y];
    }
  }
  return emptyNode;
}

function getNodeIdsOnPosition(x, y) {
    var rayCaster = new THREE.Raycaster();
    rayCaster.setFromCamera(utils.screenToNormalizedGl(x, y), $$.camera);
    var intersects = rayCaster.intersectObjects($$.scene.children);
        
    var ids = intersects.map(function(intersect){return intersect.object.userData.id;});
    return ids;
}

function getOffsetForNode(nodeIndex, x, y) {
    if (nodeIndex < 0)
        return [0, 0];

    var vec = utils.screenToWorkspace(x,y);
    var node = nodes[nodeIndex];
    
    return node.position.clone().sub(vec).toArray();
}

// export
// function dragNodes(nodeIndices, x, y) {
//   nodeIndices.forEach(function(nodeIndex) {
//     dragNode(nodeIndex, x, y);
//   });
// }

// export
function dragNode(nodeIndex, x, y) {
  var node;
  if (nodeIndex < 0)
    return;

  node = nodes[nodeIndex];
  node.moveTo(utils.screenToWorkspace(x,y));
}

module.exports = {
  render: render,
  dragNode: dragNode,
  setNodeFocused: setNodeFocused,
  setNodeSelected: setNodeSelected,
  setNodeUnselected: setNodeUnselected,
  unselectAllNodes: unselectAllNodes,
  unfocusNode: unfocusNode,
  getNodeAt: getNodeAt,
  start: start
};





// to remove
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