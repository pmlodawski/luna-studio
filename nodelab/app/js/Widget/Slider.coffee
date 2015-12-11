$$ = require('common')
config       = require('config')
createText   = require('bmfont').render
font         = require('font/LatoBlack-sdf')
textMaterial = require('font/text_material').hud
layoutText   = require('bmfont').layout

LabeledWidget = require('Widget/LabeledWidget')


class Slider extends LabeledWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height
    @bgUniforms.value = {type: 'f',  value: 0.0}

  bgShader: require('shaders/slider.frag')()

  setValue: (value) -> @bgUniforms.value.value = value

module.exports = Slider
