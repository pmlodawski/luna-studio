class BaseWidget
  constructor: (widgetId, width, height) ->
    @widgetId = widgetId
    @width    = width
    @height   = height
    @visible  = true

    @mesh     = new (THREE.Group)
    @mesh.visible = @visible && (width != 0 && height != 0)

  setSize: (width, height) ->
    @width  = width
    @height = height
    @mesh.visible = @visible && (width != 0 && height != 0)
    @relayout()

  setVisible: (vis) ->
    @visible = vis
    @mesh.visible = vis && (@width != 0 && @height != 0)

  relayout: -> null
  redrawTextures: -> null
  widgetMoved: -> null

module.exports = BaseWidget
