$$     = require('common')
config = require('config')
app    = require('app')

HTMLWidget = require ('Widget/HTMLWidget')

cloneMouseEvent      = (e) -> new MouseEvent(e.type, e)
cloneKeyboardEvent   = (e) -> new KeyboardEvent(e.type, e)
cloneMouseWheelEvent = (e) -> new WheelEvent(e.type, e)

class LongText extends HTMLWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height, true

    @text = ""
    @alignment = "left"
    @element.addClass('text-widget')

  setText: (text) ->
    @text = text
    @element.html(text)
    @relayout()

  setAlignment: (align) ->
    @alignment = align
    @relayout()

  setMonospace: (mono) ->
    if mono
      @element.addClass('monospace')
    else
      @element.removeClass('monospace')

module.exports = LongText
