$$ = require('common')
config = require('config')
createText = require('bmfont').render
font = require('font/default')
textMaterial = require('font/text_material').hud
layoutText = require('bmfont').layout

BaseWidget = require ('Widget/BaseWidget')

calculateTextWidth = (txt) -> layoutText(font: font, text: txt).width

class Label extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @alignment = 'Left'
    @text = ""
    @uniforms = {}
    @uniforms[k] = v for k, v of $$.commonUniforms

  setAlignment: (align) ->
    @alignment = align
    @setLabel @text

  setLabel: (text) ->
    @text = text
    @mesh.remove @label if @label
    if @text
      layout =
        text: text
        font: font
        align: @alignment
        mode: 'pre'
      width = layoutText(layout).width * config.fontSize
      layout.width = @width / (config.fontSize)
      width = Math.min width, @width
      geometry = createText layout
      material = textMaterial()

      @label = new THREE.Mesh(geometry, material)
      @label.scale.multiplyScalar config.fontSize
      @label.position.y = 5 + @height / 2.0
      @label.position.x = switch @alignment
        when 'Left'   then  0
        when 'Right'  then  @width - width
        when 'Center' then (@width - width) / 2.0
        else throw 'Invalid text alignment'

      @mesh.add @label

  relayout: ->
    super
    @setLabel @text
module.exports = Label
