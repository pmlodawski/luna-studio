"use strict";

var release = require('config.release');
var brunch = require('brunch');
var config;

if(brunch.env !== "production") {
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

  console.info("Logging is " + (config.logging?"enabled":"disabled"));
  window.enableLogging  = function(){
    console.info("Logging enabled! Please reload the page.");
    localStorage.setItem('logging', "true" );
  };
  window.disableLogging = function(){
    console.info("Logging disabled! Please reload the page.");
    localStorage.setItem('logging', "false");
  };
} else {
  config = release;
}

module.exports = config;