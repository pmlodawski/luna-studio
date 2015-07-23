"use strict";

var $$           = require('common'),
    config       = require('config'),
    features     = require('features'),
    brunch       = require('brunch'),
    Button       = require('button'),
    font         = require("font/LatoBlack-sdf"),
    layoutText   = require('bmfont').layout;


exports.initialize = function() {
    $$.breadcrumb = {
        mesh: new THREE.Group(),
        buttons: []
    };

    $$.sceneHUD.add($$.breadcrumb.mesh);
};

exports.addButton = function(b) {
    $$.breadcrumb.mesh.add(b.mesh);
    $$.breadcrumb.buttons.push(b);
};

exports.setButtonState = function(i, state) {
    console.log(i,state);
    $$.breadcrumb.buttons[i].uniforms.state.value = state;
}

exports.clear = function() {
    _.each($$.breadcrumb.buttons, function(c) { $$.breadcrumb.mesh.remove(c); });
    $$.breadcrumb.buttons = [];
};

exports.calculateTextWidth = function(txt) {
    return layoutText({font: font, text: txt}).width * config.fontSize;
};