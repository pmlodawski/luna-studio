$$            = require 'common'
raycaster     = require 'raycaster'
connectionPen = require 'connection_pen'
Selection     = require 'Selection'
textEditor    = require 'text_editor'

shouldRender = true
forceRedrawTextures = false

disableRightClick = -> $(document).bind 'contextmenu', -> false

initializeScenes = ->
  $$.sceneHUD = new (THREE.Scene)
  $$.registry[1] =
    mesh: $$.sceneHUD
    container: $$.sceneHUD
    widgetMoved: ->

  $$.scene = new (THREE.Scene)
  $$.registry[2] =
    mesh: $$.scene
    container: $$.scene
    widgetMoved: ->

initializeCameras = ->
  $$.camera = new (THREE.OrthographicCamera)(-500, 500, -500, 500, 0, 120)
  $$.cameraHUD = new (THREE.OrthographicCamera)(-500, 500, -500, 500, 0, 120)
  $$.camera.position.z = 100
  $$.cameraHUD.position.z = 100

initializeRenderer = ->
  $$.renderer = new (THREE.WebGLRenderer)(antialias: false)
  $$.renderer.autoClear = false
  $$.rendererMap = new (THREE.WebGLRenderTarget)(100, 100)
  $$.rendererMapCtx = $$.renderer.getContext()
  $($$.renderer.domElement).addClass 'renderer'
  document.body.appendChild $$.renderer.domElement

initHTMLCanvas = ->
  $('body').append '<div id="htmlcanvas-pan"><div id="htmlcanvas"></div></div>'
  $$.htmlCanvasPan = $('#htmlcanvas-pan')
  $$.htmlCanvas = $('#htmlcanvas')

init2DCanvas = ->
  $$.canvas2D = $('<canvas id="canvas2d" tabindex="0">')[0]
  document.body.appendChild $$.canvas2D
  $$.canvas2DCtx = $$.canvas2D.getContext('2d')

initCommonWidgets = ->
  colorId = 10
  $$.currentConnection = new Connection(3, -1, colorId)
  $$.currentConnection.setVisible false
  $$.registry[3] = $$.currentConnection
  $$.scene.add $$.currentConnection.mesh
  Selection.init()

redrawWhenFontsLoaded = ->
  try
    document.fonts.ready.then ->
      forceRedrawTextures = true
    document.fonts.onloadingdone = (fontFaceSetEvent) ->
      forceRedrawTextures = true
  catch e
    console 'failed to check for fonts', e

removeSpinner = ->
  $('#spinner').remove()

initialize = ->
  disableRightClick()
  initializeScenes()
  initializeCameras()
  initializeRenderer()
  initHTMLCanvas()
  init2DCanvas()
  initCommonWidgets()
  textEditor.init()
  redrawWhenFontsLoaded()
  removeSpinner()

redrawTextures = _.throttle((->
  console.time 'redrawTextures'
  $$.lastFactor = $$.commonUniforms.camFactor.value
  _($$.registry).each (e) ->
    if e.redrawTextures != undefined
      e.redrawTextures $$.renderer
  shouldRender = true
  console.timeEnd 'redrawTextures'
), 120)

render = ->
  if Math.abs($$.commonUniforms.camFactor.value / $$.lastFactor - 1.0) > 0.005 or forceRedrawTextures
    redrawTextures()
    forceRedrawTextures = false

  if shouldRender
    $$.commonUniforms.objectMap.value = 0
    $$.commonUniforms.antialias.value = 1
    oldCf = $$.commonUniforms.camFactor.value
    module.exports.redrawTextureCallbacks.forEach (cb) -> cb $$.renderer
    module.exports.redrawTextureCallbacks = []
    $$.renderer.setClearColor config.backgroundColor, 1
    $$.renderer.setRenderTarget()
    $$.renderer.clear()
    $$.renderer.render $$.scene, $$.camera
    $$.renderer.clearDepth()
    $$.commonUniforms.camFactor.value = 1
    $$.renderer.render $$.sceneHUD, $$.cameraHUD
    $$.commonUniforms.camFactor.value = oldCf
    raycaster.renderMap()
    raycaster.cacheMap()
    module.exports.nextFrameCallbacks.forEach (cb) -> cb()
    module.exports.nextFrameCallbacks = []
    shouldRender = false

  connectionPen.fadeCanvas()
  requestAnimationFrame render

updateHtmCanvasPanPos = (x, y, factor) ->
  $$.htmlCanvasPan.css
    left: x
    top: y
  $$.htmlCanvas.css zoom: factor

updateScreenSize = (width, height) ->
  forceRedrawTextures = true
  $$.renderer.setPixelRatio getDevicePixelRatio()
  $$.renderer.setSize width, height
  $$.rendererMap.setSize width * getDevicePixelRatio(), height * getDevicePixelRatio()
  $$.canvas2D.width = width
  $$.canvas2D.height = height
  shouldRender = true

updateCamera = (factor, left, right, top, bottom) ->
  $$.camFactor.value = factor
  $$.camera.left = left
  $$.camera.right = right
  $$.camera.top = top
  $$.camera.bottom = bottom
  shouldRender = true

updateCameraHUD = (left, right, top, bottom) ->
  $$.cameraHUD.left = left
  $$.cameraHUD.right = right
  $$.cameraHUD.top = top
  $$.cameraHUD.bottom = bottom
  shouldRender = true

removeWidget = (widgetId) ->
  widget = $$.registry[widgetId]
  if !widget
    console.error 'RemoveWidget: widget ' + widgetId + ' does not exist.'
  if widget.destructor
    widget.destructor()
  widget.mesh.parent.remove widget.mesh
  delete $$.registry[widgetId]

module.exports =
  initialize: initialize
  updateCamera: updateCamera
  updateCameraHUD: updateCameraHUD
  updateScreenSize: updateScreenSize
  updateHtmCanvasPanPos: updateHtmCanvasPanPos
  render: render
  redrawTextureCallbacks: []
  nextFrameCallbacks: []
  shouldRender: -> shouldRender = true
  removeWidget: removeWidget
