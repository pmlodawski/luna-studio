"use strict";

var font         = require("font/LatoBlack-sdf"),
    layoutText   = require('bmfont').layout;

module.exports.calculateTextWidth = function (txt) {
    return layoutText({font: font, text: txt}).width;
};
