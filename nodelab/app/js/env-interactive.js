"use strict";

module.exports = function() {
  // required for interactive
  window.app            = require('app');
  window.common         = require('common');
  window.features       = require('features');
  window.config         = require('config');
  window.breadcrumb     = require('breadcrumb');
  window.raycaster      = require('raycaster');
  window.connectionPen  = require('connection_pen');
  window.Slider         = require('Widget/Slider');
  window.Toggle         = require('Widget/Toggle');
  window.TextBox        = require('Widget/TextBox');
  window.RadioButton    = require('Widget/RadioButton');
  window.Connection     = require('Widget/Connection');
  window.textEditor     = require('text_editor');
  window.GraphNode      = require('Widget/Node');
  window.Group          = require('Widget/Group');
  window.Port           = require('Widget/Port');
  window.Button         = require('Widget/Button');
  window.Label          = require('Widget/Label');
  window.LabeledWidget  = require('Widget/LabeledWidget');
  window.ScatterPlot    = require('Widget/ScatterPlot');
  window.Icon           = require('Widget/Icon');
  window.PlotImage      = require('Widget/Image');
};
