config       = require('config')
createText   = require('bmfont').render
font         = require('font/default')
textMaterial = require('font/text_material').hud
layoutText   = require('bmfont').layout
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

  relayout: ->
    @setValueLabel @value

  setValueLabel: (text) ->
    @value = text
    @mesh.remove @valueLabel if @valueLabel

    layout =
      text: text
      font: font
      align: @alignment
      mode: 'pre'
    width = layoutText(layout).width * 0.8 * config.fontSize
    layout.width = @width / (0.8 * config.fontSize)
    width = Math.min width, @width
    geometry = createText layout
    material = textMaterial()

    @valueLabel = new THREE.Mesh(geometry, material)
    @valueLabel.scale.multiplyScalar 0.8 * config.fontSize
    @valueLabel.position.y = 5 + @height / 2.0
    @valueLabel.position.x = switch @alignment
      when 'Left'   then  0
      when 'Right'  then  @width - width
      when 'Center' then (@width - width) / 2.0
      else throw 'Invalid text alignment'

    @mesh.add @valueLabel

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
