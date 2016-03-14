$$     = require('common')
config = require('config')

BaseWidget = require ('Widget/BaseWidget')

htmlCanvas = (mesh) ->
  getTopParent = (w) ->
    p = w;
    while (p != undefined && p != null)
      w = p
      p = w.parent
    return w

  if getTopParent(mesh) != $$.sceneHUD
    $('#htmlcanvas')
  else
    $('#interface-canvas')

isVisible = (m) ->
  v = true
  while (m != undefined && m != null && v == true)
    v = m.visible
    m = m.parent
  return v


class DataFrame extends BaseWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height

    @data = ""
    @element  = $('<div class="data-frame"></div>')
    htmlCanvas(@mesh).append @element

  setData: (headers, rows) ->
    console.log(arguments)

    @element.html("");
    table = $("<table/>");
    header = $("<tr/>");
    _(headers).each (h) -> header.append($("<th>").text(h))
    table.append(header);

    _(rows).each (r) ->
      row = $("<tr/>");
      _(r).each (f) -> row.append($("<td>").text(f))
      table.append(row)

    @element.append(table)
    @relayout()

  relayout: ->
    super
    @mesh.updateMatrix()
    @mesh.updateMatrixWorld(true)
    pos = @mesh.localToWorld(new (THREE.Vector3)(0, 0, 0))
    @element.css
      left: pos.x
      top: pos.y
      width: @width
      height: @height
      textAlign: @alignment
      display: if isVisible(@mesh) then 'block' else 'none'
  widgetMoved: =>
    setTimeout((=> @relayout()), 100)
  destructor: ->
    @element.remove()



module.exports = DataFrame
