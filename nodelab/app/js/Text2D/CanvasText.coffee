fontHeightCache = {}

class CanvasText
  constructor: ->
    @textWidth = null
    @textHeight = null

    @canvas = document.createElement('canvas')
    @ctx    = @canvas.getContext('2d')

  width:  -> @canvas.width
  height: -> @canvas.height

  drawText: (text, ctxOptions) ->
    @ctx.clearRect 0, 0, @canvas.width, @canvas.height

    @ctx.font = ctxOptions.font

    @textWidth  = Math.ceil @ctx.measureText(text).width
    @textHeight = (getFontHeight @ctx.font) + 2

    @canvas.width  = THREE.Math.nextPowerOfTwo @textWidth
    @canvas.height = THREE.Math.nextPowerOfTwo @textHeight

    @ctx.font         = ctxOptions.font
    @ctx.fillStyle    = ctxOptions.fillStyle
    @ctx.textAlign    = 'left'
    @ctx.textBaseline = 'top'

    @ctx.fillText text, 0, 0
    @canvas

getFontHeight = (fontStyle) ->
  result = fontHeightCache[fontStyle]

  unless result
    body = document.getElementsByTagName('body')[0];
    dummy = document.createElement 'div'

    dummyText = document.createTextNode 'MÉq_'
    dummy.appendChild dummyText
    dummy.setAttribute 'style', 'font:' + fontStyle + ';position:absolute;top:0;left:0'
    body.appendChild dummy
    result = dummy.offsetHeight

    fontHeightCache[fontStyle] = result
    body.removeChild dummy
  result

module.exports = CanvasText
