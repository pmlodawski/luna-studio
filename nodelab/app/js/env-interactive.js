"use strict";

module.exports = function() {
  // required for interactive
  window.app           = require('app');
  window.common        = require('common');
  window.features      = require('features');
  window.config        = require('config');
  window.breadcrumb    = require('breadcrumb');
  window.raycaster     = require('raycaster');
  window.connectionPen = require('connection_pen');
  window.Slider        = require('slider');
  window.Toggle        = require('toggle');
  window.TextBox       = require('textbox');
  window.Connection    = require('connection');
  window.textEditor    = require('text_editor');
  window.GraphNode     = require('node');
  window.Port          = require('port');
};
