$$ = require('common')
config = require('config')
createText = require('bmfont').render
font = require('font/LatoBlack-sdf')
textMaterial = require('font/text_material').hud
layoutText = require('bmfont').layout

calculateTextWidth = (txt) -> layoutText(font: font, text: txt).width

class Label
  constructor: (widgetId, width, height) ->
    @widgetId = widgetId
    @width  = width
    @height = height

    @uniforms = {}
    @uniforms[k] = v for k, v of $$.commonUniforms

    @mesh = new (THREE.Group)

  setLabel: (text) ->
    @mesh.remove @label if @label

    geometry = createText(
      text: text
      font: font
      align: 'center')

    textWidth = calculateTextWidth(text) * config.fontSize
    material = textMaterial()

    @label = new (THREE.Mesh)(geometry, material)
    @label.scale.multiplyScalar config.fontSize
    @label.position.y = 15.0

    @mesh.add @label

module.exports = Label
