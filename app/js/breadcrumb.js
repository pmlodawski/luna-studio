"use strict";

var $$           = require('common'),
    config       = require('config'),
    features     = require('features'),
    brunch       = require('brunch'),
    Button       = require('button'),
    font         = require("font/LatoBlack-sdf"),
    layoutText   = require('bmfont').layout;

exports.calculateTextWidth = function(txt) {
    return layoutText({font: font, text: txt}).width;
};