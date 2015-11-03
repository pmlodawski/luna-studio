"use strict";

var release        = require('features.release');
var brunch = require('brunch');
var features;

if (brunch.env !== "production") {
  var local = {};
  var debug = require('features.debug');
  try {
    local   = require('features.local');
  } catch (e) {
    // no local overrides, skipping.
  }
  var browser =  _.chain(release)
                  .mapObject(function (value, feature) { return localStorage.getItem(feature); })
                  .pick     (function (value) { return value !== null; })
                  .mapObject(function (value) { return value === "true"; })
                  .value();

  features = _({}).defaults(browser, local, debug, release);

  var displayWarning = function (feature) {
    console.error("Unknown feature " + feature);
    console.error("Possible features " + Object.keys(release).join(", "));
  };
  features.enable  = function (feature) {
    if (release[feature] === undefined) {
      displayWarning(feature);
    } else {
      console.info("OK, enabling feature " + feature + " for you! Please reload the page.");
      localStorage.setItem(feature, "true" );
    }
  };
  features.disable = function (feature) {
    if (release[feature] === undefined) {
      displayWarning(feature);
    } else {
      console.info("OK, disabling feature " + feature + " for you! Please reload the page.");
      localStorage.setItem(feature, "true" );
    }
  };
  features.clear = function () {
    console.info("Clearing feature overrides. Please reload the page.");
    localStorage.clear();
  };
  window.features  = features;
} else {
  features = release;
}

module.exports = features;
