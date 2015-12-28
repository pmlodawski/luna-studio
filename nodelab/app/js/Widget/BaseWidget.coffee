class BaseWidget
  constructor: (widgetId, width, height) ->
    @widgetId = widgetId
    @width    = width
    @height   = height

    @mesh     = new (THREE.Group)

  setSize: (width, height) ->
    @width  = width
    @height = height
    @relayout()

  relayout: -> null

module.exports = BaseWidget
