HTMLWidget = require ('Widget/HTMLWidget')

class CodeEditor extends HTMLWidget
  constructor: (widgetId, width, height) ->
    super widgetId, width, height, false
    @initEditor()
    @relayout()

  initEditor: ->
    @editor = ace.edit @element[0]
    @editor.setTheme "ace/theme/twilight"
    @editor.getSession().setMode("ace/mode/ruby")
    @editor.renderer.setShowGutter(false);
    @editor.setValue "", -1
    @editor.$blockScrolling = Infinity

  setCode: (text) ->
    @text = text
    @editor.setValue text, -1
    @relayout()

  getCode: -> "TODO"

  relayout: ->
    super
    @editor.resize()

module.exports = CodeEditor
