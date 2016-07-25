$$ = require('common')
config       = require('config')

LabeledWidget = require('Widget/LabeledWidget')

class Slider extends LabeledWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height
    @bgUniforms.value      = {type: 'f',  value: 0.0}
    @bgUniforms.ticks      = {type: 'f',  value: 0.0}
    @bgUniforms.tickOffset = {type: 'f',  value: 20.0}
    @bgUniforms.tickSpan   = {type: 'f',  value: 20.0}

  bgShader: require('shaders/slider.frag')()

  setValue: (value) -> @bgUniforms.value.value = value

  setTicks: (visible, offset, span) ->
    @bgUniforms.ticks.value      = if visible then 1.0 else 0.0
    @bgUniforms.tickOffset.value = offset
    @bgUniforms.tickSpan.value   = span

module.exports = Slider
