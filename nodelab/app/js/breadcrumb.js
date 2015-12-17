"use strict";

var font         = require("font/default"),
    layoutText   = require('bmfont').layout;

module.exports.calculateTextWidth = function (txt) {
    return layoutText({font: font, text: txt}).width;
};
