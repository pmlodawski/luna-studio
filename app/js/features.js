"use strict";

var defaults       = require('features.default');
var localOverrides = {};

if("{!env!}" !== "production") {
  try {
    localOverrides = require('features.local');
  } catch (e) {
    // no local overrides, skipping.
  }

  var browserOverrides = _.chain(defaults)
                            .mapObject(function(value, feature) { return localStorage.getItem(feature); })
                            .pick     (function(value) { return value !== null; })
                            .mapObject(function(value) { return value === "true"; })
                            .value();

  module.exports = _({}).extend(defaults, localOverrides, browserOverrides);
} else {
  module.exports = defaults;
}