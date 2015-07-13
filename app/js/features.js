"use strict";

var release        = require('features.release');
var features;

if("{!env!}" !== "production") {
  var local = {};
  var debug = require('features.debug');
  try {
    local   = require('features.local');
  } catch (e) {
    // no local overrides, skipping.
  }
  var browser =  _.chain(release)
                  .mapObject(function(value, feature) { return localStorage.getItem(feature); })
                  .pick     (function(value) { return value !== null; })
                  .mapObject(function(value) { return value === "true"; })
                  .value();

  features = _({}).defaults(browser, local, debug, release);

  features.enable  = function(feature) { localStorage.setItem(feature, "true" ); };
  features.disable = function(feature) { localStorage.setItem(feature, "false"); };
  features.clear   = function()        { localStorage.clear();                   };
  window.features  = features;
} else {
  features = release;
}

module.exports = features;