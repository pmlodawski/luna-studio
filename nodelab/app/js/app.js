"use strict";

var $$             = require('common'),
    config         = require('config'),
    brunch         = require('brunch'),
    raycaster      = require('raycaster'),
    GraphNode      = require('Widget/Node'),
    NodeSearcher   = require('node_searcher'),
    Connection     = require('Widget/Connection'),
    Selection      = require('Selection'),
    websocket      = require('websocket'),
    textEditor     = require('text_editor'),
    connectionPen  = require('connection_pen'),
    GoogleAnalytics = require('GoogleAnalytics'),
Terminal       = window.Terminal;

console.info("Current version " + brunch.env + " " + brunch.git_commit + " build  " + brunch.build_number);
console.info("Build at " + brunch.date);

$$.currentConnection = null;
$$.websocket         = websocket();

var shouldRender = true;
var forceRedrawTextures = false;

function start() {
  $(document).ready(function (){
    if (window.already_initialized) {
      console.error("app already started");
      return;
    }
    window.already_initialized = true;
    GoogleAnalytics.startGA();
    require('env')();
    setInterval(function(){module.exports.customEvent("tick", null);}, 1000);
  });
}

function initializeGl() {
    $(document).bind('contextmenu', function () { return false; });

    $$.scene                 = new THREE.Scene();
    $$.sceneHUD              = new THREE.Scene();

    $$.registry[1] = {mesh: $$.sceneHUD, container: $$.sceneHUD, widgetMoved: function(){} };
    $$.registry[2] = {mesh: $$.scene   , container: $$.scene, widgetMoved: function(){} };


    $$.camera                = new THREE.OrthographicCamera(-500, 500, -500, 500, 0, 120);
    $$.cameraHUD             = new THREE.OrthographicCamera(-500, 500, -500, 500, 0, 120);
    $$.camera.position.z     = 100;
    $$.cameraHUD.position.z  = 100;
    $$.renderer              = new THREE.WebGLRenderer({ antialias: false});
    $$.renderer.autoClear    = false;
    $$.rendererMap           = new THREE.WebGLRenderTarget(100, 100);
    $$.rendererMapCtx        = $$.renderer.getContext();


    $('body').append('<div id="htmlcanvas-pan"><div id="htmlcanvas"></div></div>');
    $('body').append('<div id="interface-canvas"></div>');

    $$.htmlCanvasPan   = $("#htmlcanvas-pan");
    $$.htmlCanvas      = $("#htmlcanvas");
    $$.interfaceCanvas = $("#interfaceCanvas");

    $($$.renderer.domElement).addClass('renderer');

    $$.canvas2D = $('<canvas id="canvas2d" tabindex="0">')[0];
    document.body.appendChild($$.canvas2D);
    $$.canvas2DCtx = $$.canvas2D.getContext('2d');

    document.body.appendChild($$.renderer.domElement);

    initCommonWidgets();
    textEditor.init();

    $('#log').remove();
    $('#spinner').remove();

    initUserInfo();
    initDragDrop();
    try {
      document.fonts.ready.then(function () {
        forceRedrawTextures = true;
        console.log("Fonts loaded")
      });

      document.fonts.onloadingdone = function (fontFaceSetEvent) {
        forceRedrawTextures = true;
        console.log("Fonts loaded2", fontFaceSetEvent)
      };
    } catch (e) {
      console("failed to check for fonts", e);
    }
}

function initUserInfo() {
  $('body').append(require('templates/tutorial')());
  $('.startOnboarding').click(function(){
    module.exports.customEvent('startOnboarding', null);
  });

  // if (localStorage.getItem('tutorial') === "1")
  $(".tutorial-box").hide();


  var hideTutorial = function () {
    $(".tutorial-box").hide();
    $('#canvas2d').focus();
    localStorage.setItem('tutorial', "1");
  };

  $(".tutorial-box button, .tutorial-box .tutorial-bkg").click(hideTutorial);

  $(".tutorial-box").keydown(function (ev) {
    if(ev.keyCode === 27) {
      hideTutorial();
    }
  });

  $(".tutorial").click(function (e) {
    e.preventDefault();
    $(".tutorial-box").show().focus();
  });
  $('body').append('<div id="onboarding"></div>');
  for(var i = 0; i <= 14; ++i) {
    $("#onboarding").append(require('templates/onboarding/' + i));
  }
  $('#onboarding .tour__exit a').click(function() {
    module.exports.customEvent('closeOnboarding', null);
  });
  $("#onboarding").hide();
  $('#onboarding .tour').removeClass("active");
}


function showOnboarding(step) {
  $("#onboarding").show();
  console.log('onboarding',step);
  $('#onboarding .tour').removeClass("active");
  $("#onboarding .tour:nth-child(" + (step + 1) + ")").addClass("active");
}

function closeOnboarding() {
  $('#onboarding').hide();
  localStorage.setItem('onboarding', "1");
}

function initDragDrop() {
  $($$.canvas2D).on('dragover', function(ev){ ev.preventDefault(); });
  $($$.canvas2D).on('dragenter', function(ev){ ev.preventDefault(); });
  $($$.canvas2D).on('drop',  function(ev){
    var dt = ev.originalEvent.dataTransfer;
    var files = dt.files;
    ev.preventDefault();
    ev.stopPropagation();
    for(var i = 0; i < files.length; i++) {
      var reader = new FileReader();
      reader.readAsText(files[i]);
      reader.onload = function(e) {
        console.info("read", reader.result);
        module.exports.customEvent("file.import", reader.result);
      };
    }
  });
}

function initCommonWidgets() {
  var colorId = 10;
  $$.currentConnection = new Connection(3, -1, colorId);
  $$.currentConnection.setVisible(false);
  $$.registry[3] = $$.currentConnection;

  $$.scene.add($$.currentConnection.mesh);
  Selection.init()
}

var redrawTextures = _.throttle(function() {
  console.time('redrawTextures');
  $$.lastFactor = $$.commonUniforms.camFactor.value;
  _($$.registry).each(function(e){
    if (e.redrawTextures !== undefined) e.redrawTextures($$.renderer);
  });
  shouldRender = true;
  console.timeEnd('redrawTextures');
}, 120);

function render() {
  if(Math.abs($$.commonUniforms.camFactor.value/$$.lastFactor - 1.0) > 0.005 || forceRedrawTextures) {
    redrawTextures();
    forceRedrawTextures = false;
  }

  if (shouldRender) {
    $$.commonUniforms.objectMap.value = 0;
    $$.commonUniforms.antialias.value = 1;
    var oldCf = $$.commonUniforms.camFactor.value;
    module.exports.redrawTextureCallbacks.forEach(function(cb){cb($$.renderer);});
    module.exports.redrawTextureCallbacks = [];
    $$.renderer.setClearColor(config.backgroundColor, 1);
    $$.renderer.setRenderTarget();
    $$.renderer.clear();

    $$.renderer.render($$.scene, $$.camera);
    $$.renderer.clearDepth();

    $$.commonUniforms.camFactor.value = 1;
    $$.renderer.render($$.sceneHUD, $$.cameraHUD);

    $$.commonUniforms.camFactor.value = oldCf;

    raycaster.renderMap();
    raycaster.cacheMap();
    module.exports.nextFrameCallbacks.forEach(function(cb){cb();});
    module.exports.nextFrameCallbacks = [];
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
  forceRedrawTextures = true;
  $$.renderer.setPixelRatio(getDevicePixelRatio())
  $$.renderer.setSize(width, height);
  $$.rendererMap.setSize(width * getDevicePixelRatio(), height * getDevicePixelRatio());
  $$.canvas2D.width  = width;
  $$.canvas2D.height = height;

  shouldRender = true;
}

function updateCamera(factor, left, right, top, bottom) {
  $$.camFactor.value = factor;
  $$.camera.left     = left;
  $$.camera.right    = right;
  $$.camera.top      = top;
  $$.camera.bottom   = bottom;
  shouldRender    = true;
}

function updateCameraHUD(left, right, top, bottom) {
  $$.cameraHUD.left     = left;
  $$.cameraHUD.right    = right;
  $$.cameraHUD.top      = top;
  $$.cameraHUD.bottom   = bottom;
  shouldRender       = true;
}

function createPendingNode(widgetId, expr, x, y) {
  var pos = new THREE.Vector2(x, y);
  var node = new GraphNode(pos, 0, widgetId);
  node.setLabel(expr);
  node.setPending();
  $$.scene.add(node.mesh);
  $$.registry[widgetId] = node;
}



function removeWidget(widgetId) {
  var widget = $$.registry[widgetId];
  if (!widget) {
    console.error("RemoveWidget: widget " + widgetId + " does not exist.");
    return;
  }

  if (widget.destructor) {
    widget.destructor();
  }

  widget.mesh.parent.remove(widget.mesh);
  delete $$.registry[widgetId];
}


module.exports = {
  start:                    start,
  initializeGl:             initializeGl,
  render:                   render,
  updateHtmCanvasPanPos:    updateHtmCanvasPanPos,
  updateScreenSize:         updateScreenSize,
  updateCamera:             updateCamera,
  updateCameraHUD:          updateCameraHUD,
  createNodeSearcher:       require('node_searcher').create,
  destroyNodeSearcher:      require('node_searcher').destroy,
  displaySelectionBox:      Selection.show,
  hideSelectionBox:         Selection.hide,
  removeWidget:             removeWidget,
  websocket:                $$.websocket,
  displayConnectionClosedMessage:   require("BSOD").connectionClosed,
  createPendingNode:        createPendingNode,
  nodeSearcher:             function ()      { return $$.node_searcher;   },
  shouldRender:             function ()      { shouldRender = true;       },
  getJSState:               function() { return $$; },
  downloadFile:             require('DownloadFile').downloadFile,
  customEvent:              function() { },
  nextFrameCallbacks: [],
  redrawTextureCallbacks: [],
  showOnboarding: showOnboarding,
  closeOnboarding: closeOnboarding

};



window.processedEvents = [];
