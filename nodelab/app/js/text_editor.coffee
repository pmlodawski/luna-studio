$$     = require('common')
ace    = window.ace
editor = undefined

init = ->
  $$.editorContainer = $('<div id="editorContainer"/>')

  # setupSaveButton()

  $('body').append $$.editorContainer
  editorDiv = $('<div id="editor"/>')
  $$.editorContainer.append editorDiv
  editor = ace.edit('editor')
  editor.setTheme 'ace/theme/twilight'
  editor.setReadOnly true
  editor.getSession().setMode 'ace/mode/ruby'
  editor.$blockScrolling = Infinity
  $$.editor = editor

  # $$.editor.getSession().on 'change', ->
  #   if $$.editor.isFocused()
  #     saveButton.attr 'disabled', false

# setupSaveButton = ->
#   saveButton = $('<button type="button" disabled>Apply changes</button>')
#   $$.editorContainer.append saveButton
#
#   saveChanges = ->
#     module.exports.callback $$.editor.getValue()
#     saveButton.attr 'disabled', true
#
#   saveButton.click saveChanges

setText = (text) -> editor.setValue text, -1
setWidth = (w) -> $$.editorContainer.css width: w
setVisible = (v) -> if v then $$.editorContainer.show() else $$.editorContainer.hide()
module.exports =
  init: init
  setText: setText
  setVisible: setVisible
  setWidth: setWidth
  callback: -> console.error 'TextEditor callback not registered'
