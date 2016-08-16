config       = require('config')
createText   = require('bmfont').render

textAlign    = require('Text2D/textAlign')
Text2D       = require('Text2D/Text2D')

BaseWidget   = require ('Widget/BaseWidget')

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


class TextBox extends BaseWidget
  constructor:  (widgetId, width, height) ->
    super widgetId, width, height

    @value     = ''
    @alignment = 'Left'

  destructor: ->
    @input.remove() if @input

  relayout: ->
    @setValueLabel @value

  setValueLabel: (text) ->
    @value = text
    @mesh.remove @valueLabel if @valueLabel
    align = switch @alignment
      when 'Left'   then textAlign.bottomLeft
      when 'Center' then textAlign.bottomCenter
      when 'Right'  then textAlign.bottomRight
      else throw 'Invalid text alignment'

    @valueLabel = new Text2D(@value, { align: align, fontSize: 10.4, zoom: $$.commonUniforms.camFactor.value, maxWidth: @width })
    @valueLabel.position.x = switch @alignment
      when 'Left'   then 0
      when 'Center' then @width / 2.0
      when 'Right'  then @width
      else throw 'Invalid text alignment'

    @valueLabel.position.y = @height / 2.0

    @mesh.add @valueLabel

  redrawTextures: ->
    @valueLabel.setZoom $$.commonUniforms.camFactor.value if @valueLabel

  setAlignment: (align) ->
    @alignment = align
    @setValueLabel @value

  startEditing: (value) ->
    @input.remove() if @input

    input = $('<input type="text" class="widget"/>')

    pos = @mesh.localToWorld(new (THREE.Vector3)(0, 0, 0))
    @valueLabel.visible = false
    @input = input
    input.css
      left: pos.x
      top: pos.y
      width: @width
      height: @height
      textAlign: @alignment
    input.val value
    setTimeout (-> input.select()), 10

    saveChanges = ->
      evt = new Event('keydown')
      evt.which = evt.keyCode = 13
      document.getElementById('canvas2d').dispatchEvent evt

    cancelChanges = ->
      evt = new Event('keydown')
      evt.which = evt.keyCode = 27
      document.getElementById('canvas2d').dispatchEvent evt

    input.on 'keydown', (ev) ->
      switch ev.keyCode
        when 13
          saveChanges()
          ev.preventDefault()
        when 27
          cancelChanges()
          ev.preventDefault()
      ev.stopPropagation()

    input.on 'blur', (ev) ->
      saveChanges()
      ev.stopPropagation()

    htmlCanvas(@mesh).append input
    setTimeout (-> input.focus()), 30

  doneEditing: ->
    @input.remove() if @input
    @valueLabel.visible = true
    @input = null

module.exports = TextBox
