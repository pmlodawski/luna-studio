"use strict";

var $ = require('jquery');
var _ = require('underscore');


function doSearch(ns, query) {
  var items = [
    {name: "Lorem",                       type: "module",   module: "Duis",        highlight: [{start:1, length: 3}]},
    {name: "TroLoLoMaLo",                 type: "module",   module: "Excepteur",   highlight: [{start:5, length: 2}, {start:9, length: 1}, {start:13, length: 2}]},
    {name: "aute",                        type: "function", module: "Ipsum",       highlight: [{start:0, length: 1}, {start:3, length: 1}]},
    {name: "commodo_tro_lo_alo_lo",       type: "function", module: "Excepteur",   highlight: [{start:5, length: 2}, {start:9, length: 1}, {start:13, length: 2}]},
    {name: "cupidatat",                   type: "function", module: "Ipsum",       highlight: [{start:3, length: 2}, {start:5, length: 1}, {start:8, length: 2}]},
    {name: "eiusmod",                     type: "function", module: "Excepteur",   highlight: [{start:1, length: 2}, {start:8, length: 1}, {start:8, length: 2}]},
    {name: "elit",                        type: "function", module: "Ipsum",       highlight: [{start:0, length: 2}, {start:3, length: 1}]},
    {name: "labore",                      type: "function", module: "Excepteur",   highlight: [{start:3, length: 2}]},
    {name: "magna",                       type: "function", module: "Duis",        highlight: [{start:2, length: 4}]},
    {name: "minim",                       type: "function", module: "Duis",        highlight: [{start:1, length: 3}]},
    {name: "mollit",                      type: "function", module: "",        highlight: [{start:0, length: 2}, {start:4, length: 1}]},
    {name: "voluptate",                   type: "function", module: "",        highlight: [{start:4, length: 1}, {start:7, length: 1}, {start:9, length: 2}]},
  ];
  
  if(ns !== "") {
    _(items).each(function(el){el.module = ns + "." + el.module;});
  }
  return items;
}

function doSearchTree(ns) {
  var items = [
    {name: "Lorem",                       type: "module",   module: "Duis",        highlight: [{start:1, length: 3}]},
    {name: "TroLoLoMaLo",                 type: "module",   module: "Excepteur",   highlight: [{start:5, length: 2}, {start:9, length: 1}, {start:13, length: 2}]},
    {name: "aute",                        type: "function", module: "Ipsum",       highlight: [{start:0, length: 1}, {start:3, length: 1}]},
    {name: "commodo_tro_lo_alo_lo",       type: "function", module: "Excepteur",   highlight: [{start:5, length: 2}, {start:9, length: 1}, {start:13, length: 2}]},
    {name: "cupidatat",                   type: "function", module: "Ipsum",       highlight: [{start:3, length: 2}, {start:5, length: 1}, {start:8, length: 2}]},
    {name: "eiusmod",                     type: "function", module: "Excepteur",   highlight: [{start:1, length: 2}, {start:8, length: 1}, {start:8, length: 2}]},
    {name: "elit",                        type: "function", module: "Ipsum",       highlight: [{start:0, length: 2}, {start:3, length: 1}]},
    {name: "labore",                      type: "function", module: "Excepteur",   highlight: [{start:3, length: 2}]},
    {name: "magna",                       type: "function", module: "Duis",        highlight: [{start:2, length: 4}]},
    {name: "minim",                       type: "function", module: "Duis",        highlight: [{start:1, length: 3}]},
    {name: "mollit",                      type: "function", module: "Duis",        highlight: [{start:0, length: 2}, {start:4, length: 1}]},
    {name: "voluptate",                   type: "function", module: "Duis",        highlight: [{start:4, length: 1}, {start:7, length: 1}, {start:9, length: 2}]},
  ];
  
  if(ns !== "") {
    _(items).each(function(el){el.module = ns;});
  } else {
    _(items).each(function(el){el.module = "";});
  }
  
  return items;
}

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

function removeNS(item, prefix) {
  var clean;
  if(item.substring(0, prefix.length) === prefix) {
    clean = item.substring(prefix.length);
    if(clean[0] === '.') {
      clean = clean.slice(1);
    }
    return clean;
  } else {
    return item;
  }
}

function joinName(ns, item) {
  if(ns === "")
    return item;
  else
    return ns + "." + item;
}

function NodeSearcher() {
  this.el = $('<div/>').addClass('node-searcher');

  this.initSearchbox();
  this.search("","");
}

NodeSearcher.prototype.currentSelection = function() {
  return this.currentColumn.find('li.active');
};

NodeSearcher.prototype.select = function(nextSelection) {
  if(this.currentSelection()[0] === nextSelection[0]) return;
  
  if(this.currentSelection().parent().parent() !== nextSelection.parent().parent()) {
    this.selectColumn(nextSelection.parent().parent());
  }
  
  this.currentSelection().parent().parent().nextAll().remove();
  this.currentSelection().removeClass('active');
  nextSelection.addClass('active');

  if(!nextSelection.hasClass('query') && nextSelection.data('match').type === 'module') {
    this.openColumn();
  } 
};

NodeSearcher.prototype.onKeyUpDown = function(event, up) {
  var currentSelection, nextSelection;

  currentSelection  = this.currentSelection(); 
  if(up) {
    nextSelection = currentSelection.prev();
  } else {
    nextSelection = currentSelection.next();
  }

  if(nextSelection.length > 0) {
    this.select(nextSelection);
    
    var visibleBottom  = this.currentColumn.scrollTop() + this.currentColumn.innerHeight() - 20;
    var selectedBottom = this.currentColumn.scrollTop() + nextSelection.position().top + nextSelection.height();
    
    if(visibleBottom < selectedBottom) {
      this.currentColumn.animate({scrollTop: selectedBottom - this.currentColumn.innerHeight() + 30 }, 50);
    }
    
    var visibleTop  = this.currentColumn.scrollTop();
    var selectedTop = this.currentColumn.scrollTop() + nextSelection.position().top;
    if(visibleTop > selectedTop) {
      this.currentColumn.animate({scrollTop: selectedTop - 10}, 50);
    }
  }

  event.preventDefault();
  event.stopPropagation();  
};

NodeSearcher.prototype.onBackspace = function(event) {
  var val = this.searchbox.val();
  var idx, ns, query;

  if(val === "") {
    idx = this.ns.substring(0, this.ns.length).lastIndexOf('.');
    query = this.ns.substring(idx+1, this.ns.length);
    ns = this.ns.substring(0, idx);
    this.search(ns, query);
    event.preventDefault();
  } 
  
  event.stopPropagation();
};

NodeSearcher.prototype.isSearchboxActive = function() {
  return this.searchrow.hasClass('active');  
};

NodeSearcher.prototype.onKeyLeft = function(event) {
  if(!this.isSearchboxActive()) {    
    if(this.currentColumn.prev().length !== 0){
      this.currentColumn.nextAll().remove();
      this.selectColumn(this.currentColumn.prev());
      event.preventDefault();
    }
  }
  event.stopPropagation();
};

NodeSearcher.prototype.onKeyRight = function(event) { 
  var next;
  if(!this.isSearchboxActive()) {
    next = this.currentColumn.next(); 
    if(next.length > 0) {
      this.selectColumn(next);
    }
    event.preventDefault();
  }
  event.stopPropagation();
};

NodeSearcher.prototype.selectNS = function(ns) {
  var displayNS = ns;
  this.ns = ns;
  
  if(displayNS !== "") {
    displayNS += ".";
  }
  this.searchns.text(displayNS);
};


NodeSearcher.prototype.search = function(ns, query) {
  this.searchbox.val(query);  
  this.searchbox.focus();
  this.selectNS(ns);
  this.performSearch();
};

NodeSearcher.prototype.performSearch = function() {
  var results;
  if(this.ns === "" && this.searchbox.val() === "") {
    this.firstColumn.removeClass('types');
  } else {
    this.firstColumn.addClass('types');
  }

  if(this.searchbox.val() === "") {
    results = doSearchTree(this.ns);
  } else {
    results = doSearch(this.ns, this.searchbox.val());
  }

  this.searchrow.addClass('active');    
  this.displaySearchResults(results);
};


NodeSearcher.prototype.onInput = function() {
  var val = this.searchbox.val();
  var ns, query;
  var idx = val.indexOf('.');
  if(idx !== -1 && idx > 0) {
    ns = joinName(this.ns, val.substring(0, idx));
    query = val.substring(idx+1);
    this.search(ns, query);      
  } else {
    this.performSearch();
  }
};

NodeSearcher.prototype.onEnter = function() {
  var current, data;
  
  if(this.searchrow.hasClass('active')) {
    this.createNode(joinName(this.ns, this.searchbox.val()));
  } else {
    current = this.currentSelection();
    data    = current.data('match');
    
    if(data.type === 'module') {
      this.search(joinName(data.module, data.name), "");
    } else {
      this.createNode(joinName(data.module, data.name));
    }
  }
};

NodeSearcher.prototype.initSearchbox = function() {
  var _this = this;

  var firstColumn = $('<div/>').addClass('column').addClass('current');
  firstColumn.items = $('<ul/>');
  firstColumn.append(firstColumn.items);
  firstColumn.items.append('<li class="active query"><span class="query-ns"></span><input type="text" class="query"/></li>');
  
  this.searchrow = firstColumn.items.find('li');
  this.searchbox = firstColumn.items.find('input.query');
  this.searchns  = firstColumn.items.find('span.query-ns');
  
  this.ns = "";

  this.el.append(firstColumn);
  this.currentColumn = firstColumn;
  this.firstColumn = firstColumn;

  this.searchbox.on('input', function() {
    _this.onInput();
  });

  this.searchbox.on('keydown', function(ev) {
    switch(ev.keyCode){
      case 8: {
        _this.onBackspace(ev);
        break;
      }
      case 38: { //up
        _this.onKeyUpDown(ev, true);
        break;
      }
      case 40: { // down
        _this.onKeyUpDown(ev, false);
        break;
      }
      case 37: { // left
        _this.onKeyLeft(ev);
        break;
      }
      case 39: { // right
        _this.onKeyRight(ev);
        break;
      }
      case 13: { //enter
        _this.onEnter(ev);
        break;
      }
    }
  });
  
  this.el.on("mousemove", ".column ul li", function(ev) {
    _this.select($(ev.currentTarget));
  });
  this.el.on("click", ".column ul li.result", function(ev) {
    _this.select($(ev.currentTarget));    
    _this.onEnter();
  });
};

NodeSearcher.prototype.createNode = function(expression) {
  alert(expression);
};


  
NodeSearcher.prototype.selectColumn = function(column) {
  this.currentColumn.removeClass('current');
  this.currentColumn = column;
  this.currentColumn.addClass('current');
  this.currentColumn.nextAll().find("li").removeClass("active");
  if(this.currentColumn.find("li.active").length === 0) {
    this.currentColumn.find("li:first-child").addClass("active");
  }  
};

NodeSearcher.prototype.openColumn = function() {  
  var column = $('<div/>').addClass('column');
  column.items = $('<ul/>');
  column.append(column.items);

  this.el.append(column);
  this.displayTree(doSearchTree(this.currentSelection().data('match').module +"."+ this.currentSelection().data('match').name), column.items);
};

NodeSearcher.prototype.updateNSWidth = function() {
  this.firstColumn.find('.ns').css({minWidth: 90+this.searchns.width()});
};

NodeSearcher.prototype.displaySearchResults = function(results) {
  this.firstColumn.nextAll().remove();
  this.currentColumn = this.firstColumn;
  this.displayResults(results, this.firstColumn.items);
  this.updateNSWidth();
};

NodeSearcher.prototype.displayResults = function(results, ul) {
  var _this = this;
  ul.find('li.result').remove();

  _(results).each(function(item) {
    var li = $('<li/>').addClass('result');
    var ns = $("<span/>").addClass('ns').text(removeNS(item.module, _this.ns));
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