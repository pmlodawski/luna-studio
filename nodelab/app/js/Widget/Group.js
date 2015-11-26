"use strict";

function Group(widgetId) {
  this.widgetId = widgetId;
  this.mesh = this.container = new THREE.Group();
}

module.exports = Group;
