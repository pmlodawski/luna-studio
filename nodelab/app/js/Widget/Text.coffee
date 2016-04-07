$$     = require('common')
config = require('config')

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


class LongText extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @text = ""
    @alignment = "left"
    @element  = $('<div class="text-widget"></div>')
    htmlCanvas(@mesh).append @element

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
    setTimeout((=> @relayout()), 0)
  destructor: ->
    @element.remove()



module.exports = LongText
