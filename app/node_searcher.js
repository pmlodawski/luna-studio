"use strict";

var $ = require('jquery');
var _ = require('underscore');

var searcherResults = [
  {name: "1234567890",          module: "Ipsum",       highlight: [
      {start:0, length: 1},
      {start:2, length: 1},
      {start:4, length: 1},
      {start:6, length: 1},
      {start:8, length: 1}
  ]},
  {name: "elit",          module: "Ipsum",       highlight: [{start:0, length: 2}, {start:3, length: 1}]},
  {name: "aute",          module: "Ipsum",       highlight: [{start:0, length: 1}, {start:3, length: 1}]},
  {name: "cupidatat",     module: "Ipsum",       highlight: [{start:3, length: 2}, {start:5, length: 1}, {start:8, length: 2}]},
  {name: "eiusmod",       module: "Excepteur",   highlight: [{start:1, length: 2}, {start:8, length: 1}, {start:8, length: 2}]},
  {name: "commodo_tro_lo_alo_lo",       module: "Excepteur",   highlight: [{start:5, length: 2}, {start:9, length: 1}, {start:13, length: 2}]},
  {name: "labore",        module: "Excepteur",   highlight: [{start:3, length: 2}]},
  {name: "mollit",        module: "Duis",        highlight: [{start:0, length: 2}, {start:4, length: 1}]},
  {name: "voluptate",     module: "Duis",        highlight: [{start:4, length: 1}, {start:7, length: 1}, {start:9, length: 2}]},
  {name: "magna",         module: "Duis",        highlight: [{start:2, length: 4}]},
  {name: "minim",         module: "Duis",        highlight: [{start:1, length: 3}]}
];

var treeResuts = [
  
];

function highlightText(name, highlight) {
  highlight.push({start: name.length, length: 0});
  
  return _(highlight).foldl(function(acc, el) {
    if(acc.pos < el.start) {
      acc.elems.push(name.substring(acc.pos, el.start));
    }
    acc.elems.push($("<em/>").text(name.substring(el.start, el.start+el.length)));
    acc.pos = el.start + el.length;
      
    return acc; 
  }, {pos: 0, elems: []});
}

function NodeSearcher() {
  this.el = $('<div/>').addClass('node-searcher');
  this.browser = $('<div/>').addClass('browser');
  this.el.append(this.browser);
  this.columns = []; 

  var firstColumn = $('<div/>').addClass('column');
  firstColumn.items = $('<ul/>');
  firstColumn.append(firstColumn.items);
  firstColumn.items.append('<li class="active"><span class="query-ns empty"></span><input type="text" class="query"/></li>');
  this.searchbox = firstColumn.items.find('input.query');
  this.searchns = firstColumn.items.find('span.query-ns');
  this.ns = "";

  this.columns.push(firstColumn);
  this.browser.append(firstColumn);
  
  this.initSearchbox();
}

NodeSearcher.prototype.initSearchBox = function() {
  var _this = this;

  this.searchbox.on('input', function() {
    var val = _this.searchbox.val();
    if(_this.ns === "" && val === ""){
      _this.displayResults([]);
      _this.searchns.addClass('empty');
    } else {
      var idx = val.indexOf('.');
      if(idx > -1) {
        if(_this.ns !== "") _this.ns += ".";
        _this.ns += val.substring(0, idx);
        _this.searchns.text(_this.ns);
        _this.searchbox.val(val.substring(idx+1));
      }
      _this.displayResults(_(searcherResults).sample(10 - _this.searchbox.val().length));
      _this.searchns.removeClass('empty');
      _this.updateNSWidth();
    }
  });
  
  this.searchbox.on('keydown', function(ev) {
    var val = _this.searchbox.val();
    if(ev.keyCode === 8 && val === "") {
      var idx = _this.ns.lastIndexOf('.');
      _this.searchbox.val(_this.ns.substring(idx+1));
      _this.ns = _this.ns.substring(0, idx);
      _this.searchns.text(_this.ns);      
      _this.updateNSWidth();
      ev.preventDefault();
    }
  });
});
  
NodeSearcher.prototype.updateNSWidth = function() {
  this.columns[0].find('.ns').css({minWidth: this.searchns.width()});
};

NodeSearcher.prototype.displayResults = function(results) {
  var ul = this.columns[0].items;
  ul.find('li.result').remove();
  
  _(results).each(function(item) {
    var el = $('<li/>').addClass('result');
    var ns = $("<span/>").addClass('ns').text(item.module);
    var name = $("<span/>").addClass('fname');

    ul.append(el);
    el.append(ns);
    el.append(name);

    _(highlightText(item.name, item.highlight).elems).each(function(part){
      name.append(part)      
    });
    
  });
};

module.exports = NodeSearcher;