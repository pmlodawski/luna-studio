var $ = require('jquery')
var THREE = require('three')
var _ = require('underscore')
var fs = require('./shaders/node.frag')
var vs = require('./shaders/node.vert')

// script to be inclued in index.html and referenced from Main.hs
var scene, camera, renderer;
var geometry, material, mesh;

var nodes = [];    // array of nodes with nodes
var nodeIndex = [];    // array: nodeIndex[plane.id] == nodes.indexOf(plane)
var scene;      // main scene
var renderer;       // main renderer
var camera;     // main camera

var attributes = [];    // array used by shaders
var uniforms = [];
var commonUniforms =  {
  color: { type: 'v4', value: new THREE.Vector4(1.0, 1.0, 1.0, .9) },
  camPan: {type: 'v2',value: new THREE.Vector2(0.0, 0.0)},
  camFactor: {type: 'f',value: 1.0},
  screenSize: {type: 'v2', value: new THREE.Vector2(1280.0, 800.0)}
}
var screenSize = commonUniforms.screenSize.value;
var camFactor = commonUniforms.camFactor
var camPan = commonUniforms.camPan.value

var posValue = [];  // array used by shaders;
                    // posValue[i] contains global position of node in nodes[i]

var draggedIndex;   // index of object being dragged
var dragging;       // indicates whether node is being dragged

var zOrderDiv = 10000.0;
var currentMazZ = 0.0;
var maxZ = (Math.pow(2, 31) - 1) / zOrderDiv;


var THREE;
//// STATS
var stats = {begin: function(){}, end: function(){}};
//
// stats = new Stats();
// stats.setMode( 0 ); // 0: fps, 1: ms, 2: mb
//
// // align top-left
// stats.domElement.style.position = 'absolute';
// stats.domElement.style.left = '0px';
// stats.domElement.style.top = '0px';
//
// document.body.appendChild( stats.domElement );



//// END STATS


function start() {
  $(document).ready(function(){
    THREE = require('three')
    init();
    create(50);
    render();
  });
  
}
function render() {
  stats.begin();
  renderer.render(scene, camera);
  stats.end()
  requestAnimationFrame(render);
}

function init() {
    var camDist = window.innerHeight/2;
    scene = new THREE.Scene();
    // camera = new THREE.PerspectiveCamera(90, window.innerWidth / window.innerHeight, 0.1, 1000);
    var width = window.innerWidth;
    var height = window.innerHeight;
    camera = new THREE.OrthographicCamera(width / - 2, width / 2, height / 2, height / - 2, 1, 1000);

    renderer = new THREE.WebGLRenderer({ antialias: true });
    $(renderer.domElement).addClass('renderer')
    renderer.setClearColor( 0x1a1a1a, 1 );

    document.body.appendChild( renderer.domElement );

    camera.position.z = camDist;


    document.addEventListener( 'keydown', onDocumentKeyDown, false );
    $(window).resize(setWindowSize)
    setWindowSize()


    setup_pan_and_drag()
}

function setup_pan_and_drag() {
  var dragMode = null;
  var mouse = new THREE.Vector2(0,0)
  var mouseStart = new THREE.Vector2(0,0)
  $(document).bind('contextmenu', function(){return false})
  $(document).mousedown(function(event) {
    event.preventDefault()
    switch(event.which) {
      case 2: dragMode = 'pan'; break;
      case 3: dragMode = 'zoom'; break;
      default: dragMode = null;
    }
    mouse.x = event.clientX;
    mouse.y = event.clientY;

    mouseStart.x = (mouse.x - screenSize.x/2)/camFactor.value;
    mouseStart.y = (-mouse.y + screenSize.y/2)/camFactor.value;
    console.log(mouseStart)
  });

  $(document).mousemove(function(event) {
    switch(dragMode){
      case 'pan': {
        camPan.x -= (event.clientX - mouse.x)/camFactor.value
        camPan.y += (event.clientY - mouse.y)/camFactor.value
        reconfigure_camera()
        break;
      }
      case 'zoom': {
        var deltaX = (event.clientX - mouse.x)
        var deltaY = -(event.clientY - mouse.y)
        var factor = (deltaX+deltaY)/512
        if(updateCamFactor(camFactor.value * (1+factor))) {
          camPan.x += mouseStart.x * factor;
          camPan.y += mouseStart.y * factor
          mouseStart.x -= mouseStart.x * factor;
          mouseStart.y -= mouseStart.y * factor
        }
        reconfigure_camera()
        break;
      }
    }
    mouse.x = event.clientX;
    mouse.y = event.clientY;
  });

  $(document).mouseup(function(event) {
    dragMode = null;
    event.preventDefault()
  })
}

function setWindowSize(){
    var w = window.innerWidth //renderer.domElement.scrollWidth
    var h = window.innerHeight //renderer.domElement.scrollHeight

    commonUniforms.screenSize.value.x = w
    commonUniforms.screenSize.value.y = h
    renderer.setSize( w, h );
    reconfigure_camera()
}

function updateCamFactor(val) {
  camFactor.value = Math.max(0.2, Math.min(4.0, val))
  return (camFactor.value == val)
}

function onDocumentKeyDown( event )
{
  switch(event.keyCode)
  {
    case 187: { // plus
      updateCamFactor(camFactor.value * 1.1)
      reconfigure_camera()
      break
    }
    case 189: { // minus
      updateCamFactor(camFactor.value  / 1.1)
      reconfigure_camera()
      break
    }
    case 37: { // left
      camPan.x -= 10/camFactor.value
      reconfigure_camera()
      break
    }
    case 38: { // top
      camPan.y += 10/camFactor.value
      reconfigure_camera()
      break
    }
    case 39: { // right
      camPan.x += 10/camFactor.value
      reconfigure_camera()
      break
    }
    case 40: { // bottom
      camPan.y -= 10/camFactor.value
      reconfigure_camera()
      break
    }

  }
  //   console.log(event.keyCode);
}

function reconfigure_camera() {
  var w = window.innerWidth //renderer.domElement.scrollWidth
  var h = window.innerHeight //renderer.domElement.scrollHeight

  // commonUniforms.screenSize.value.x = w
  // commonUniforms.screenSize.value.y = h
  // renderer.setSize( w, h );

  $("#inputcanvas").css({top: h/2+camPan.y*camFactor.value, left: w/2-camPan.x*camFactor.value})
  $("#zoomcanvas").css({zoom: camFactor.value})


  camera.left = - w / 2 / camFactor.value + camPan.x;
  camera.right = w / 2 / camFactor.value + camPan.x;
  camera.top = h / 2 / camFactor.value + camPan.y;
  camera.bottom = - h / 2 / camFactor.value + camPan.y;

  camera.updateProjectionMatrix();
}

function create(number) {
    var posX;
    var posY;
    var i;

    // nodes
    var width = 60;     // nodes' width
    var height = 60;    // nodes' height

    for (var i = 0; i < number; i++)
    {
        posX = (Math.random() - 0.5) * (window.innerWidth - width);  // possible X coords of planes
        posY = (Math.random() - 0.5) * (window.innerHeight - height);  // possible Y coords of planes

        posValue[i] = new THREE.Vector2(posX, posY);

        attributes[i] = {
            pos: {
                type: 'v2',
                value: [
                    posValue[i],
                    posValue[i],
                    posValue[i],
                    posValue[i]
                ]
            }
        };

        uniforms[i] = {
            selected: { type: "i", value: 0 }
        };

        Object.keys(commonUniforms).forEach(function(k) { // copy common uniforms
          uniforms[i][k] = commonUniforms[k]
        })

        nodes[i] = new THREE.Mesh(
            new THREE.PlaneGeometry(width, height),
            new THREE.ShaderMaterial( {
                uniforms: uniforms[i],
                attributes: attributes[i],
                vertexShader:   vs(),
                fragmentShader: fs(),
                transparent: true,
                blending: THREE.NormalBlending
            })
        );
        nodeIndex[nodes[i].id] = i;
        nodes[i].position.x = posX;
        nodes[i].position.y = posY;


        // var rect = new THREE.Mesh(
 //          new THREE.PlaneGeometry(20, 30),
 //          new THREE.MeshBasicMaterial( { color: 0xff0000 })
 //        )
 //
 //        rect.position.x = -10;
 //        rect.position.y = 0;
 //        rect.position.z = 0.00009;
 //
 //        nodes[i].userData.rect = rect
 //        nodes[i].add(rect)
 //
        // nodes[i].userData.textBox = $("<input/>");
        // nodes[i].userData.textBox.attr({ type: 'text', value: "n# " + i});
        // nodes[i].userData.textBox.css({left: posX-20, top: -posY-10});
        // nodes[i].userData.textBox.appendTo('#zoomcanvas')

        scene.add(nodes[i]);
        // console.log("nodes " + nodeIndex[nodes[i].id] + " " + nodes[i].id);
    }
    assignZs();
}

function getNormalizedPosition(x, y) {
    var mouse = new THREE.Vector2();
    mouse.x =  (x / screenSize.x) * 2 - 1;
    mouse.y = -(y / screenSize.y) * 2 + 1;
    return mouse;
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
    // console.log("setNodeFocused " + nodeId + " to " + focused)
    uniforms[nodeId].selected.value = 2
    moveToTopZ(nodeId)
}

// export
function setNodeSelected(nodeId) {
    // console.log("setNodeSelected " + nodeId)
    uniforms[nodeId].selected.value = 1
    moveToTopZ(nodeId)
}

// export
function setNodeUnselected(nodeId) {
    // console.log("setNodeUnselected " + nodeId + " to " + selected)
    uniforms[nodeId].selected.value = 0
}

// export
function unselectAllNodes() {
    // console.log("unselectAllNodes")
    for (var i = 0; i < nodes.length; i++) {
        uniforms[i].selected.value = 0;
    }
}

// export
function unfocusNode() {
    // console.log("unselectAllNodes")
    for (var i = 0; i < nodes.length; i++) {
        if (uniforms[i].selected.value == 2)
            uniforms[i].selected.value = 1
    }
}

// export
// function toggleNodeSelection(nodeId) {
//     console.log("toggleNodeSelection " + nodeId)
//     uniforms[nodeId].selected.value = 1 - uniforms[nodeId].selected.value
// }

function assignZs() {
    var sortedNodes = nodes.slice(0);
    sortedNodes.sort(function(nodeA, nodeB) {
        return nodeA.position.z - nodeB.position.z;
    } );
    for (var i = 0; i < sortedNodes.length; i++)
        sortedNodes[i].position.z = i / zOrderDiv;
    currentMazZ = sortedNodes[sortedNodes.length - 1].position.z;
}

function moveToTopZ(nodeId) {
    var currentNode = nodes[nodeId]
    currentNode.position.z = currentMazZ + 1.0 / zOrderDiv;
    currentMazZ = currentNode.position.z;
    if (currentMazZ > maxZ)
        assignZs();
}

// export
function addNodeIdWithOffsetOnPosition(x, y) {
    var emptyNode = [-1, 0, 0];
    // var nodeIds = getNodeIdsOnPosition(x, y);
    // // unselectAll();
    // if (nodeIds.length == 0) return emptyNode;
    // for (var i = 0; i < nodeIds.length; i++) {
    //     var nodeId = nodeIds[i];
    //     var xy = getOffsetForNode(nodeId, x, y);
    //     if (isSelectedNodeIdOnPosition(nodeId, xy)) {
    //         rearrangeNodesZOrder(nodes[nodeId]);
    //         selectNode(nodeId);
    //         return [nodeId, xy[0], xy[1]];
    //     }
    // }
    return emptyNode;
}

// export
function getNodeAt(x, y) {
    var emptyNode = [-1, 0, 0, 0]
    var nodeIds = getNodeIdsOnPosition(x, y)
    // unselectAllNodes();
    if (nodeIds.length == 0) return emptyNode
    for (var i = 0; i < nodeIds.length; i++) {
        var nodeId = nodeIds[i]
        var node = nodes[nodeId]
        var offset = getOffsetForNode(nodeId, x, y)
        if (isSelectedNodeIdOnOffset(nodeId, offset)) {
            // selectNode(nodeId);
            // console.log("nodeId " + nodeId + " selected " + uniforms[nodeId].selected.value)
            xy = tran(node.position)
            // dragNode2(nodeId, xy[0], xy[1])
            return [nodeId,
                    uniforms[nodeId].selected.value,
                    xy[0], xy[1]]
        }
    }
    return emptyNode
}

function tran(pos) {
    var x = pos.x
    var y = pos.y
    // var nsx =  (x - screenSize.x/2)/camFactor.value + camPan.x
    // var nsy = (-y + screenSize.y/2)/camFactor.value + camPan.y
    var sx =  x + (renderer.domElement.width)  / 2
    var sy = -y + (renderer.domElement.height) / 2
    return [sx, sy]
}

function getNodeIdsOnPosition(x, y) {
    var rayCaster = new THREE.Raycaster();
    rayCaster.setFromCamera(getNormalizedPosition(x, y), camera);
    var intersects = rayCaster.intersectObjects(scene.children);
    var ids = [];
    for (var i = 0; i < intersects.length; i++) {
        var currentObject = intersects[i].object.id;
        if (currentObject != undefined) {  // is node type
            var idx = nodeIndex[currentObject];
            ids.push(idx);
        }
    }
    // console.log("Found ids " + ids.length);
    return ids;
}

function getOffsetForNode(nodeIndex, x, y) {
    if (nodeIndex < 0)
        return [0, 0];
    var nsx =  (x - screenSize.x/2)/camFactor.value + camPan.x;
    var nsy = (-y + screenSize.y/2)/camFactor.value + camPan.y;
    var node = nodes[nodeIndex];
    var dx = node.position.x - nsx;
    var dy = node.position.y - nsy;
    return [dx, dy];
}

function dragNodes(nodeIndices, x, y) {
    for (var i = 0; i < nodeIndices.length; i++) {
        dragNode(nodeIndices[i], x, y)
    }
}

// export
function dragNode(nodeIndex, x, y) {
    if (nodeIndex < 0)
        return
    // else return
    var sx = x + window.scrollX;
    var sy = y + window.scrollY;


    var x =  sx - (renderer.domElement.width)  / 2
    var y = -sy + (renderer.domElement.height) / 2


    nodes[nodeIndex].position.x = x / camFactor.value + camPan.x;
    nodes[nodeIndex].position.y = y / camFactor.value + camPan.y;

    posValue[nodeIndex].x = nodes[nodeIndex].position.x;
    posValue[nodeIndex].y = nodes[nodeIndex].position.y;

    // nodes[nodeIndex].userData.textBox.css({left: posValue[nodeIndex].x-20, top: -posValue[nodeIndex].y-10})

    // posValue[nodeIndex].x =  sx;
    // posValue[nodeIndex].y = -sy + renderer.domElement.height;

    // console.log("xy " + x + " " + y);
    // console.log("scr " + window.scrollX + " " + window.scrollY);
    // console.log("pla " + nodes[nodeIndex].position.x + " " + nodes[nodeIndex].position.y);

    attributes[nodeIndex].pos.needsUpdate = true;
    // uniforms[nodeIndex].pos.value[0] = posValue[nodeIndex].x;
    // uniforms[nodeIndex].pos.value[1] = posValue[nodeIndex].y;
    // uniforms[nodeIndex].pos.needsUpdate = true;
}

function displayWindowInfo() {
    console.log("innerWidth: "  + window.innerWidth);
    console.log("innerHeight: " + window.innerHeight);
    console.log("screenX: " + window.screenX);
    console.log("screenY: " + window.screenY);
    console.log("window scrollX: " + window.scrollX);
    console.log("window scrollY: " + window.scrollY); 
}

console.log('dupa2')

module.exports = {
  init: init,
  render: render,
  create: create,
  dragNode: dragNode,
  setNodeFocused: setNodeFocused,
  setNodeSelected: setNodeSelected,
  setNodeUnselected: setNodeUnselected,
  unselectAllNodes: unselectAllNodes,
  unfocusNode: unfocusNode,
  getNodeAt: getNodeAt,
  start: start
}




