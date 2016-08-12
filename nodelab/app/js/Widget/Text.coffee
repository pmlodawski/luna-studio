$$     = require('common')
config = require('config')
app    = require('app')

BaseWidget = require ('Widget/BaseWidget')

htmlCanvas = (mesh) ->
  getTopParent = (w) ->
    p = w;
    while (p != undefined && p != null)
      w = p
      p = w.parent
    return w

  if getTopParent(mesh) != $$.sceneHUD
    $('#htmlcanvas')
  else
    $('#interface-canvas')

isVisible = (m) ->
  v = true
  while (m != undefined && m != null && v == true)
    v = m.visible
    m = m.parent
  return v

cloneMouseEvent      = (e) -> new MouseEvent(e.type, e)
cloneKeyboardEvent   = (e) -> new KeyboardEvent(e.type, e)
cloneMouseWheelEvent = (e) -> new WheelEvent(e.type, e)

class LongText extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @text = ""
    @alignment = "left"
    @element  = $('<div class="text-widget"></div>')
    htmlCanvas(@mesh).append @element
    passThroughHandlerMouse = (evt) =>
      evt.preventDefault()
      $('#canvas2d')[0].dispatchEvent(cloneMouseEvent(evt.originalEvent))

    passThroughHandlerKbd = (evt) =>
      evt.preventDefault()
      $('#canvas2d')[0].dispatchEvent(cloneKeyboardEvent(evt.originalEvent))


    @element.on 'mousemove', passThroughHandlerMouse
    @element.on 'mouseup',   passThroughHandlerMouse
    @element.on 'mousedown', passThroughHandlerMouse
    @element.on 'keyup',     passThroughHandlerKbd
    @element.on 'keydown',   passThroughHandlerKbd
    @element.on 'keypress',  passThroughHandlerKbd
    @element.on 'mousewheel', (evt) =>
      evt.stopPropagation()
      if evt.ctrlKey
        evt.preventDefault()
        $('#canvas2d')[0].dispatchEvent(cloneMouseWheelEvent(evt.originalEvent))


  setText: (text) ->
    @text = text
    @element.html(text)
    @relayout()

  setAlignment: (align) ->
    @alignment = align
    @relayout()

  relayout: ->
    super
    @mesh.updateMatrix()
    @mesh.updateMatrixWorld(true)
    pos = @mesh.localToWorld(new (THREE.Vector3)(0, 0, 0))
    @element.css
      left: pos.x
      top: pos.y
      width: @width
      height: @height
      textAlign: @alignment
      display: if isVisible(@mesh) then 'block' else 'none'
  widgetMoved: =>
    app.nextFrameCallbacks.push(=> @relayout())
  destructor: ->
    @element.remove()



module.exports = LongText
