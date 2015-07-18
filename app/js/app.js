"use strict";

var FunctionNode = require('function_node').FunctionNode,
    $$           = require('common'),
    config       = require('config'),
    features     = require('features'),
    NodeSearcher = require('node_searcher'),
    brunch        = require('brunch');

console.info("Current version " + brunch.env + " " + brunch.git_commit);
console.info("Build at " + brunch.date);

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
    $$.renderer             = new THREE.WebGLRenderer({ antialias: true });
		$$.renderer.autoClear   = false;


    $$.renderer.setClearColor(config.backgroundColor, 1);
    initSelectBox();
    addVersionToHud();
    $($$.renderer.domElement).addClass('renderer');
    document.body.appendChild($$.renderer.domElement);
}

function addVersionToHud() {
  var createText   = THREE_TEXT;
  var font         = require("font/LatoBlack-sdf");
  var textMaterial = require('font/text_material').hud;

  var geom = createText({
    text: "Build at " + brunch.date,
    font: font,
    width: 5000,
    align: 'left'
  });

  var obj = new THREE.Mesh(geom, textMaterial);
  // obj.rotation.x = 180 * Math.PI/180;
  obj.scale.multiplyScalar(config.fontSize);
  obj.position.y = 20;

  $$.sceneHUD.add(obj);
}

function initSelectBox() {
  var geometry = new THREE.Geometry();
  geometry.vertices.push(
    new THREE.Vector3(0,0,0),
    new THREE.Vector3(1,1,0),
    new THREE.Vector3(0,1,0),
    new THREE.Vector3(1,0,0)
  );
  geometry.faces.push(new THREE.Face3(0, 2, 1), new THREE.Face3(0, 1, 3));
  var mesh = new THREE.Mesh(
			geometry,
			new THREE.ShaderMaterial( {
				uniforms: {
					visible: { type: 'f', value: 0 },
					size: { type: 'v3', value: new THREE.Vector3(0,0,1) },
					color: { type: 'v4', value: new THREE.Vector4(0.85, 0.55, 0.1,0.3) }
				},
				vertexShader:   require('shaders/select.vert')(),
				fragmentShader: require('shaders/color.frag')(),
				transparent: true,
				blending: THREE.NormalBlending,
        side:           THREE.DoubleSide
  	})
	);
  mesh.position.z = 100;
  $$.selectBox = mesh;
  $$.scene.add( mesh );
}


function render() {
  $$.renderer.clear();
  $$.renderer.render($$.scene, $$.camera);
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
  nodes[i] = node;
  node.label(expr);
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

function displaySelectBox(x,y,w,h) {
  $$.selectBox.material.uniforms.visible.value = 1;
  $$.selectBox.position.x = x;
  $$.selectBox.position.y = y;
  $$.selectBox.material.uniforms.size.value.x = w;
  $$.selectBox.material.uniforms.size.value.y = h;
}

function hideSelectBox() {
  $$.selectBox.material.uniforms.visible.value = 0;
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
  updateCameraHUD: updateCameraHUD,
  start: start,
  createNodeSearcher: createNodeSearcher,
  destroyNodeSearcher: destroyNodeSearcher,
  displaySelectBox: displaySelectBox,
  hideSelectBox: hideSelectBox,
  nodeSearcher: function() { return $$.node_searcher; }
};


