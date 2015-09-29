"use strict";

var $$        = require('common');

var editor;

function init() {
  $$.editorContainer = $('<div id="editorContainer"/>');
  $('body').append($$.editorContainer);
  var editorDiv = $('<div id="editor"/>')
  $$.editorContainer.append(editorDiv);
  editor = ace.edit("editor");
  editor.setTheme("ace/theme/twilight");
  editor.getSession().setMode("ace/mode/ruby");
  $$.editor = editor;

  $$.editor.getSession().on('change', _.debounce(function(e){
    module.exports.callback($$.editor.getValue());
  }, 500));
};

function setText(text) {
  editor.setValue(text);
}

module.exports = {
  init: init,
  setText: setText,
  callback: function(t) { console.error("TextEditor callback not registered"); }
}
