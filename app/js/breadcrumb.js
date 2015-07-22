"use strict";

var $$           = require('common'),
    config       = require('config'),
    features     = require('features'),
    brunch       = require('brunch'),
    Button       = require('button'),
    font         = require("font/LatoBlack-sdf"),
    layoutText   = require('bmfont').layout;


exports.initialize = function() {
    $$.breadcrumb = new THREE.Group();
    $$.sceneHUD.add($$.breadcrumb);
};

exports.addButton = function(b) {
    $$.breadcrumb.add(b.mesh);
};

exports.setButtonState = function(i, state) {
    $$.breadcrumb.children[i].material.uniforms.state.value = state;
}

exports.clear = function() {
    _.each(_.clone($$.breadcrumb.children), function(c) { $$.breadcrumb.remove(c); });
};

exports.calculateTextWidth = function(txt) {
    return layoutText({font: font, text: txt}).width * config.fontSize;
};