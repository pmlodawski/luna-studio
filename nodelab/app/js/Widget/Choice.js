"use strict";

// var $$ = require('common');
// var vs = require('shaders/toggle.vert')();
// var fs = require('shaders/toggle.frag')();
// var config = require('config');
//
function Choice(widgetId, width, height) {
  this.widgetId = widgetId;
  this.mesh = this.container = new THREE.Group();
}

module.exports = Choice;
