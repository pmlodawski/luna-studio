$$ = require('common')
config = require('config')
createText = require('bmfont').render
font = require('font/LatoBlack-sdf')
textMaterial = require('font/text_material').hud
layoutText = require('bmfont').layout

BaseWidget = require ('Widget/BaseWidget')

calculateTextWidth = (txt) -> layoutText(font: font, text: txt).width

class Label extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @uniforms = {}
    @uniforms[k] = v for k, v of $$.commonUniforms

  setLabel: (text) ->
    @text = text
    @mesh.remove @label if @label

    geometry = createText(
      text: text
      font: font
      align: 'center'
      width: @width / config.fontSize
      mode:  'pre')

    material = textMaterial()

    @label = new (THREE.Mesh)(geometry, material)
    @label.scale.multiplyScalar config.fontSize
    @label.position.y = 15.0

    @mesh.add @label

  relayout: ->
    super
    @setLabel @text
module.exports = Label
