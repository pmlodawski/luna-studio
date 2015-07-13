"use strict";

var release = require('config.release');
var config;

if("{!env!}" !== "production") {
  var local = {};
  var debug = require('config.debug');
  try {
    local   = require('config.local');
  } catch (e) {
    // no local overrides, skipping.
  }

  var browser = {};
  if(localStorage.getItem('logging') !== null) {
    browser.logging = (localStorage.getItem('logging') === "true");
  }

  config = _({}).defaults(browser, local, debug, release);

  console.log("Logging is " + (config.logging?"enabled":"disabled"));
  window.enableLogging  = function(){ localStorage.setItem('logging', "true" ); };
  window.disableLogging = function(){ localStorage.setItem('logging', "false"); };
} else {
  config = release;
}

module.exports = config;