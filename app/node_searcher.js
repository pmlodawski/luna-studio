"use strict";

var $ = require('jquery');
var _ = require('underscore');

var searcherResults = [
  {name: "elit",                        type: "function", module: "Ipsum",       highlight: [{start:0, length: 2}, {start:3, length: 1}]},
  {name: "aute",                        type: "function", module: "Ipsum",       highlight: [{start:0, length: 1}, {start:3, length: 1}]},
  {name: "cupidatat",                   type: "function", module: "Ipsum",       highlight: [{start:3, length: 2}, {start:5, length: 1}, {start:8, length: 2}]},
  {name: "eiusmod",                     type: "function", module: "Excepteur",   highlight: [{start:1, length: 2}, {start:8, length: 1}, {start:8, length: 2}]},
  {name: "commodo_tro_lo_alo_lo",       type: "function", module: "Excepteur",   highlight: [{start:5, length: 2}, {start:9, length: 1}, {start:13, length: 2}]},
  {name: "labore",                      type: "function", module: "Excepteur",   highlight: [{start:3, length: 2}]},
  {name: "mollit",                      type: "function", module: "Duis",        highlight: [{start:0, length: 2}, {start:4, length: 1}]},
  {name: "voluptate",                   type: "function", module: "Duis",        highlight: [{start:4, length: 1}, {start:7, length: 1}, {start:9, length: 2}]},
  {name: "magna",                       type: "function", module: "Duis",        highlight: [{start:2, length: 4}]},
  {name: "minim",                       type: "function", module: "Duis",        highlight: [{start:1, length: 3}]},
  {name: "Lorem",                       type: "module",   module: "Duis",        highlight: [{start:1, length: 3}]},
  {name: "elit",                        type: "function", module: "Ipsum",       highlight: [{start:0, length: 2}, {start:3, length: 1}]},
  {name: "aute",                        type: "function", module: "Ipsum",       highlight: [{start:0, length: 1}, {start:3, length: 1}]},
  {name: "cupidatat",                   type: "function", module: "Ipsum",       highlight: [{start:3, length: 2}, {start:5, length: 1}, {start:8, length: 2}]},
  {name: "eiusmod",                     type: "function", module: "Excepteur",   highlight: [{start:1, length: 2}, {start:8, length: 1}, {start:8, length: 2}]},
  {name: "commodo_tro_lo_alo_lo",       type: "function", module: "Excepteur",   highlight: [{start:5, length: 2}, {start:9, length: 1}, {start:13, length: 2}]},
  {name: "labore",                      type: "function", module: "Excepteur",   highlight: [{start:3, length: 2}]},
  {name: "mollit",                      type: "function", module: "Duis",        highlight: [{start:0, length: 2}, {start:4, length: 1}]},
  {name: "voluptate",                   type: "function", module: "Duis",        highlight: [{start:4, length: 1}, {start:7, length: 1}, {start:9, length: 2}]},
  {name: "magna",                       type: "function", module: "Duis",        highlight: [{start:2, length: 4}]},
  {name: "minim",                       type: "function", module: "Duis",        highlight: [{start:1, length: 3}]},
  {name: "Lorem",                       type: "module",   module: "Duis",        highlight: [{start:1, length: 3}]},
  {name: "elit",                        type: "function", module: "Ipsum",       highlight: [{start:0, length: 2}, {start:3, length: 1}]},
  {name: "aute",                        type: "function", module: "Ipsum",       highlight: [{start:0, length: 1}, {start:3, length: 1}]},
  {name: "cupidatat",                   type: "function", module: "Ipsum",       highlight: [{start:3, length: 2}, {start:5, length: 1}, {start:8, length: 2}]},
  {name: "eiusmod",                     type: "function", module: "Excepteur",   highlight: [{start:1, length: 2}, {start:8, length: 1}, {start:8, length: 2}]},
  {name: "commodo_tro_lo_alo_lo",       type: "function", module: "Excepteur",   highlight: [{start:5, length: 2}, {start:9, length: 1}, {start:13, length: 2}]},
  {name: "labore",                      type: "function", module: "Excepteur",   highlight: [{start:3, length: 2}]},
  {name: "mollit",                      type: "function", module: "Duis",        highlight: [{start:0, length: 2}, {start:4, length: 1}]},
  {name: "voluptate",                   type: "function", module: "Duis",        highlight: [{start:4, length: 1}, {start:7, length: 1}, {start:9, length: 2}]},
  {name: "magna",                       type: "function", module: "Duis",        highlight: [{start:2, length: 4}]},
  {name: "minim",                       type: "function", module: "Duis",        highlight: [{start:1, length: 3}]},
  {name: "Lorem",                       type: "module",   module: "Duis",        highlight: [{start:1, length: 3}]},
  {name: "elit",                        type: "function", module: "Ipsum",       highlight: [{start:0, length: 2}, {start:3, length: 1}]},
  {name: "aute",                        type: "function", module: "Ipsum",       highlight: [{start:0, length: 1}, {start:3, length: 1}]},
  {name: "cupidatat",                   type: "function", module: "Ipsum",       highlight: [{start:3, length: 2}, {start:5, length: 1}, {start:8, length: 2}]},
  {name: "eiusmod",                     type: "function", module: "Excepteur",   highlight: [{start:1, length: 2}, {start:8, length: 1}, {start:8, length: 2}]},
  {name: "commodo_tro_lo_alo_lo",       type: "function", module: "Excepteur",   highlight: [{start:5, length: 2}, {start:9, length: 1}, {start:13, length: 2}]},
  {name: "labore",                      type: "function", module: "Excepteur",   highlight: [{start:3, length: 2}]},
  {name: "mollit",                      type: "function", module: "Duis",        highlight: [{start:0, length: 2}, {start:4, length: 1}]},
  {name: "voluptate",                   type: "function", module: "Duis",        highlight: [{start:4, length: 1}, {start:7, length: 1}, {start:9, length: 2}]},
  {name: "magna",                       type: "function", module: "Duis",        highlight: [{start:2, length: 4}]},
  {name: "minim",                       type: "function", module: "Duis",        highlight: [{start:1, length: 3}]},
  {name: "Lorem",                       type: "module",   module: "Duis",        highlight: [{start:1, length: 3}]},
  {name: "TroLoLoMaLo",                 type: "module",   module: "Excepteur",   highlight: [{start:5, length: 2}, {start:9, length: 1}, {start:13, length: 2}]},
];

function highlightText(name, highlight) {
  highlight.push({start: name.length, length: 0});

  return _(highlight).foldl(function(acc, el) {
    if(acc.pos < el.start) {
      acc.elems.push(name.substring(acc.pos, el.start));
    }
    var part = name.substring(el.start, el.start+el.length);
    if(part.length > 0) {
      acc.elems.push($("<em/>").text(part));
    }
    acc.pos = el.start + el.length;

    return acc;
  }, {pos: 0, elems: []});
}

function NodeSearcher() {
  this.el = $('<div/>').addClass('node-searcher');
  this.browser = $('<div/>').addClass('browser');
  this.el.append(this.browser);
  this.columns = [];

  this.initSearchbox();
}

NodeSearcher.prototype.initSearchbox = function() {
  var _this = this;

  var firstColumn = $('<div/>').addClass('column');
  firstColumn.items = $('<ul/>');
  firstColumn.append(firstColumn.items);
  firstColumn.items.append('<li class="active"><span class="query-ns empty"></span><input type="text" class="query"/></li>');
  // firstColumn.data('offset', '0')
  this.searchrow = firstColumn.items.find('li');
  this.searchbox = firstColumn.items.find('input.query');
  this.searchns = firstColumn.items.find('span.query-ns');
  this.ns = "";

  this.columns.push(firstColumn);
  this.browser.append(firstColumn);
  this.currentColumn = firstColumn;

  this.searchbox.on('input', function() {
    var val = _this.searchbox.val();
    if(_this.ns === "" && val === ""){
      _this.displaySearchResults([]);
      _this.searchrow.addClass('active', _this.columns[0]);
      _this.columns[0].removeClass('types');
    } else {
      var idx = val.indexOf('.');
      if(idx !== -1 && idx > 0) {
        _this.ns += val.substring(0, idx+1);
        _this.searchns.text(_this.ns);
        _this.searchbox.val(val.substring(idx+1));
      }
      _this.displaySearchResults(_(searcherResults).sample(10 - _this.searchbox.val().length));
      _this.searchrow.addClass('active');

      _this.columns[0].addClass('types');
      _this.updateNSWidth();
    }
  });

  this.searchbox.on('keydown', function(ev) {
    var val = _this.searchbox.val();
    var current, next, data;

    switch(ev.keyCode){
      case 8: {
        if(val === "") {
          var idx = _this.ns.substring(0, _this.ns.length-1).lastIndexOf('.');
          _this.searchbox.val(_this.ns.substring(idx+1, _this.ns.length-1));
          _this.ns = _this.ns.substring(0, idx+1);
          _this.searchns.text(_this.ns);
          _this.updateNSWidth();
          ev.preventDefault();
        }
        break;
      }
      case 38: { //up
        current  = _this.currentColumn.find('li.active');
        next = current.prev();

        if(next.length > 0) {
          current.removeClass('active');
          next.addClass('active');
        }

        ev.preventDefault();
        ev.stopPropagation();
        break;
      }
      case 40: { // down
        current  = _this.currentColumn.find('li.active');
        next = current.next();

        if(next.length > 0) {
          current.removeClass('active');
          next.addClass('active');
        }

        ev.preventDefault();
        ev.stopPropagation();
        break;
      }
      case 37: { // left
        if(_this.columns.length > 1){
          ev.preventDefault();
          // _this.columns[_this.columns.length-1].remove();
          _this.currentColumn = _this.currentColumn.prev();
          _this.columns.pop().remove();
        }
        ev.stopPropagation();
        break;
      }
      case 39: { // right
        if(!_this.searchrow.hasClass('active')) {
          ev.preventDefault();
          current  = _this.currentColumn.find('li.active');
          data = current.data('match');
          if(data.type === "module") {
            _this.openColumn(current);
          }
        }
        ev.stopPropagation();
        break;
      }
      case 13: { //enter
        if(_this.searchrow.hasClass('active')) {
          _this.createNode(_this.searchbox.val());
        } else {
          current  = _this.currentColumn.find('li.active');
          data = current.data('match');
          if(data.module !== "")
            _this.createNode(data.module + "." + data.name);
          else
            _this.createNode(data.name);
        }
        break;
      }
    }
  });
};

NodeSearcher.prototype.createNode = function(expression) {
  alert(this.ns+expression);
};

NodeSearcher.prototype.openColumn = function(node) {
  var column = $('<div/>').addClass('column');
  column.items = $('<ul/>');
  column.append(column.items);

  this.columns.push(column);
  this.browser.append(column);
  this.displayTree(_(searcherResults).sample(Math.random()*30+1), column.items);
  column.items.find("li:first-child").addClass('active');
  this.currentColumn = column;
};

NodeSearcher.prototype.updateNSWidth = function() {
  this.columns[0].find('.ns').css({minWidth: 100+this.searchns.width()});
};

NodeSearcher.prototype.displaySearchResults = function(results) {
  this.browser.find(".column + .column").remove();
  this.currentColumn = this.columns[0];
  this.displayResults(results, this.columns[0].items);
}

NodeSearcher.prototype.displayResults = function(results, ul) {
  ul.find('li.result').remove();

  _(results).each(function(item) {
    var li = $('<li/>').addClass('result');
    var ns = $("<span/>").addClass('ns').text(item.module);
    var name = $("<span/>").addClass('fname');

    li.data('match', item);
    li.addClass(item.type);

    ul.append(li);
    li.append(ns);
    li.append(name);

    _(highlightText(item.name, item.highlight).elems).each(function(part){
      name.append(part);
    });

  });
};

NodeSearcher.prototype.displayTree = function(results, ul) {
  ul.find('li.result').remove();

  _(results).each(function(item) {
    var li = $('<li/>').addClass('result');
    var name = $("<span/>").addClass('fname').text(item.name);

    li.data('match', item);
    li.addClass(item.type);

    ul.append(li);
    li.append(name);
  });
};

module.exports = NodeSearcher;