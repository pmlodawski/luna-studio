"use strict";

var $$             = require('common'),
    config         = require('config'),
    brunch         = require('brunch'),
    raycaster      = require('raycaster'),
    GraphNode      = require('Widget/Node'),
    NodeSearcher   = require('node_searcher'),
    Connection     = require('Widget/Connection'),
    SelectionBox   = require('Widget/SelectionBox'),
    websocket      = require('websocket'),
    textEditor     = require('text_editor'),
    connectionPen  = require('connection_pen'),
    Terminal       = window.Terminal;

console.info("Current version " + brunch.env + " " + brunch.git_commit + " build  " + brunch.build_number);
console.info("Build at " + brunch.date);

$$.currentConnection = null;
$$.selectionBox      = null;
$$.websocket         = websocket();

var shouldRender = true;

function start() {
  $(document).ready(function (){
    if (window.already_initialized) {
      console.error("app already started");
      return;
    }
    window.already_initialized = true;
    startGA();
    require('env')();
    window.h$errorMsg = displayAppCrashed;
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

    initTerminal();
    initUserInfo();
    initDragDrop();
}

function initUserInfo() {
  $('body').append(require('templates/logo')());
  $('body').append(require('templates/tutorial')());
  $('#startOnboarding').click(function(){
    module.exports.customEvent('startOnboarding', null);;
  })

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
    $("#onboarding").append(require('templates/onboarding/' + i))
  }
  $('#onboarding .tour__exit a').click(function() {
    module.exports.customEvent('closeOnboarding', null);
  })
  $("#onboarding").hide();
  $('#onboarding .tour').removeClass("active");
}


function showOnboarding(step) {
  $("#onboarding").show();
  console.log('onboarding',step)
  $('#onboarding .tour').removeClass("active");
  $("#onboarding .tour:nth-child(" + (step + 1) + ")").addClass("active");
}

function closeOnboarding() {
  $('#onboarding').hide();
  localStorage.setItem('onboarding', "1");
}

function initTerminal() {
  $('body').append('<div id="termContainer"><button id="termClose">Close</button><div id="term"></div></div>');
  $$.term = new Terminal({
    useStyle: true,
    rows: 20,
    cols: 40
  });

  $$.term.open($("#term")[0]);
  $$.term.write('\x1b[31mWelcome to Nodelab!\x1b[m\r\n');
  $("#termClose").click(function (){
    $('#termContainer').css({height: "0px"});
  });
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
  $$.selectionBox      = new SelectionBox();

  $$.scene.add($$.currentConnection.mesh);
  $$.scene.add($$.selectionBox.mesh);
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
  if(Math.abs($$.commonUniforms.camFactor.value/$$.lastFactor - 1.0) > 0.005) {
    redrawTextures();
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
  $$.renderer.setSize(width, height);
  $$.rendererMap.setSize(width, height);
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

function createNodeSearcher(expression, nodeId, left, top, command) {
  var ns;
  destroyNodeSearcher();
  ns = new NodeSearcher();
  $$.node_searcher = ns;
  $('body').append(ns.el);
  ns.init(nodeId, command);
  ns.el.css({left: left, top: top});
  if (expression)
    ns.setExpression(expression);
  return ns;
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

function writeToTerminal(str) {
  $('#termContainer').css({height: "300px"});
  $$.term.write(str);
}

var cleanupApp = function () {
  $("body > div, body > canvas").remove();
};

var displayRejectedMessage = function () {
  cleanupApp();
  $('body').append(require('templates/rejected')());
};

var displayConnectionClosedMessage = function () {
  if($('#rejected').length === 0) {
    cleanupApp();
    $('body').append(require('templates/connection_closed')());
    ga('send', 'event', 'Diagnostic', 'ConnectionLost');
  }
};

var displayAppCrashed = function (pat) {
  // poor man's vprintf
  var str = pat;
  for(var i=1;i<arguments.length;i++) {
    str = str.replace(/%s/, arguments[i]);
  }

  console.error('Haskell crashed', str);

  $('body').append(require('templates/bsod')({message: str}));
  ga('send', 'event', 'Diagnostic', 'BSOD', str);
};

var startGA = function (){
  var enabled = $$.isGAEnabled();
  if(enabled) {
    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m);
    })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

    ga('create', config.gaTrackingId, 'auto');
    ga('send', 'pageview', '/b' + brunch.build_number);
  } else {
    window.ga = function() {};
  }
};

var downloadFile = (function () {
    var a = document.createElement("a");
    document.body.appendChild(a);
    a.style = "display: none";
    return function (data, fileName) {
        var blob = new Blob([data], {type: "octet/stream"}),
            url = window.URL.createObjectURL(blob);
        a.href = url;
        a.download = fileName;
        a.click();
        window.URL.revokeObjectURL(url);
    };
}());


module.exports = {
  start:                    start,
  initializeGl:             initializeGl,
  render:                   render,
  updateHtmCanvasPanPos:    updateHtmCanvasPanPos,
  updateScreenSize:         updateScreenSize,
  updateCamera:             updateCamera,
  updateCameraHUD:          updateCameraHUD,
  createNodeSearcher:       createNodeSearcher,
  destroyNodeSearcher:      destroyNodeSearcher,
  displaySelectionBox:      displaySelectionBox,
  hideSelectionBox:         hideSelectionBox,
  removeWidget:             removeWidget,
  websocket:                $$.websocket,
  displayRejectedMessage:   displayRejectedMessage,
  displayConnectionClosedMessage:   displayConnectionClosedMessage,
  createPendingNode:        createPendingNode,
  nodeSearcher:             function ()      { return $$.node_searcher;   },
  shouldRender:             function ()      { shouldRender = true;       },
  writeToTerminal:          writeToTerminal,
  displayAppCrashed:        displayAppCrashed,
  getJSState:               function() { return $$; },
  downloadFile:             downloadFile,
  customEvent:              function() { },
  nextFrameCallbacks: [],
  redrawTextureCallbacks: [],
  showOnboarding: showOnboarding,
  closeOnboarding: closeOnboarding

};



window.processedEvents = [];
