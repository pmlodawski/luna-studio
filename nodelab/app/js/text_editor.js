"use strict";

var $$        = require('common');

var editor;

function init() {
  $$.editorContainer = $('<div id="editorContainer"/>');
  var saveButton = $('<button type="button" disabled>Apply changes</button>');
  $$.editorContainer.append(saveButton);
  $('body').append($$.editorContainer);
  var editorDiv = $('<div id="editor"/>')
  $$.editorContainer.append(editorDiv);
  editor = ace.edit("editor");
  editor.setTheme("ace/theme/twilight");
  editor.getSession().setMode("ace/mode/ruby");
  $$.editor = editor;

  var saveChanges = function() {
    module.exports.callback($$.editor.getValue());
    saveButton.attr('disabled', true);
  }
  saveButton.click(saveChanges);

  $$.editor.getSession().on('change', function() {
    if ($$.editor.isFocused()) {
      saveButton.attr('disabled', false);
    }
  });
};

function setText(text) {
  editor.setValue(text);
}

module.exports = {
  init: init,
  setText: setText,
  callback: function(t) { console.error("TextEditor callback not registered"); }
}
