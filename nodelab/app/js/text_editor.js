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
  editor.setReadOnly(true);

  var noPropagate = function(ev) { ev.stopPropagation(); };
  $$.editorContainer.on('keydown',   noPropagate);
  $$.editorContainer.on('keyup',     noPropagate);
  $$.editorContainer.on('keypress',  noPropagate);
  $$.editorContainer.on('mousedown', noPropagate);
  $$.editorContainer.on('mouseup',   noPropagate);
  $$.editorContainer.on('mousemove', noPropagate);
  $$.editorContainer.on('click',     noPropagate);
  $$.editorContainer.on('dblclick',  noPropagate);
};

function setText(text) {
  editor.setValue(text);
}

module.exports = {
  init: init,
  setText: setText
}
