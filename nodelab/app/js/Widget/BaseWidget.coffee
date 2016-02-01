class BaseWidget
  constructor: (widgetId, width, height) ->
    @widgetId = widgetId
    @width    = width
    @height   = height

    @mesh     = new (THREE.Group)
    @mesh.visible = (width != 0 && height != 0)

  setSize: (width, height) ->
    @width  = width
    @height = height
    @mesh.visible = (width != 0 && height != 0)
    @relayout()

  relayout: -> null
  redrawTextures: -> null

module.exports = BaseWidget
