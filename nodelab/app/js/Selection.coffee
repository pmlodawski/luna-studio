SelectionBox = require 'Widget/SelectionBox'
selectionBox = new SelectionBox()

init = ->
  $$.scene.add selectionBox.mesh

show = (x0, y0, x1, y1) ->
  selectionBox.setPos x0, y0, x1, y1
  selectionBox.show()

hide = -> selectionBox.hide()

module.exports =
  init: init
  show: show
  hide: hide
