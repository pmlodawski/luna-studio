HTMLWidget = require 'Widget/HTMLWidget'
app        = require 'app'

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
    @editor.on('change', (delta) => @onChange(delta))
    @editor.on('blur', (ev) => @onBlur(ev))

  setCode: (text) ->
    @text = text
    @editor.setValue text, -1
    @relayout()

  onChange: (delta) -> app.customEvent("widget", { "_widgetId": @widgetId, "_payload": "CodeEditorChange"})
  onBlur:   (ev)    -> app.customEvent("widget", { "_widgetId": @widgetId, "_payload": "CodeEditorBlur"})

  getCode: -> @editor.getValue()

  relayout: ->
    super
    @editor.resize()

module.exports = CodeEditor
