"use strict";

var $      = require('jquery')
  , _      = require('underscore')
  , config = require('./config')
;



function doSearch(expr) {
  console.log("ExprSearch: " + expr);
  // var ns = splitExpression(expr).prefix;
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
    {name: "mollit",                      type: "function", module: "",            highlight: [{start:0, length: 2}, {start:4, length: 1}]},
    {name: "voluptate",                   type: "function", module: "",            highlight: [{start:4, length: 1}, {start:7, length: 1}, {start:9, length: 2}]},
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
    {name: "mollit",                      type: "function", module: "",            highlight: [{start:0, length: 2}, {start:4, length: 1}]},
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
    {name: "mollit",                      type: "function", module: "",            highlight: [{start:0, length: 2}, {start:4, length: 1}]},
    {name: "voluptate",                   type: "function", module: "",            highlight: [{start:4, length: 1}, {start:7, length: 1}, {start:9, length: 2}]},
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
    {name: "mollit",                      type: "function", module: "",            highlight: [{start:0, length: 2}, {start:4, length: 1}]},
    {name: "voluptate",                   type: "function", module: "",            highlight: [{start:4, length: 1}, {start:7, length: 1}, {start:9, length: 2}]},
  ];

  _(items).each(function(el){
    if(el.module !== "")
      el.fullName = el.module + "." + el.name;
    else
      el.fullName = el.name;
  });
  return _.shuffle(items);
}
function doSearchTree(expr) {
  console.log("TreeSearch: " + expr);
  var ns = splitExpression(expr).prefix;
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
  _(items).each(function(el){
    if(el.module !== "")
      el.fullName = el.module + "." + el.name;
    else
      el.fullName = el.name;
  });
  return _.shuffle(items);
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

function shouldSplit(query) {
  return (query.indexOf('.') > -1 || query.indexOf(' ') > -1);
}

function splitExpression(expr) {
  var prefix, query;
  var idx = Math.max(expr.lastIndexOf('.'), expr.lastIndexOf(' '));
  prefix = expr.substring(0, idx+1);
  query  = expr.substring(idx+1);
  return {prefix: prefix, query: query};
}

function NodeSearcher() {
  this.el = $('<div/>').addClass('node-searcher');
  this.prefix = "";
  this.initSearchbox();
  this.setExpression("");
}

NodeSearcher.prototype.expression = function() {
  return this.prefix + this.searchbox.val();
};

NodeSearcher.prototype.setExpression = function(expr) {
  var split = splitExpression(expr);
  this.prefix = split.prefix;
  this.searchbox.val(split.query);
  this.searchns.text(split.prefix);
  this.updatePrefixWidth();
  this.performSearch();
};

NodeSearcher.prototype.appendExpression = function(expr) {
  this.setExpression(this.prefix + expr);
};

NodeSearcher.prototype.currentSelection = function() {
  return this.currentColumn.find('.item.active');
};

NodeSearcher.prototype.select = function(newSelection) {
  if(this.currentSelection().is(newSelection)) return;

  var inSameColumn = this.currentSelection().parents('.column').is(newSelection.parents('.column'));

  if(!inSameColumn) {
    this.selectColumn(newSelection.parents('.column'));
  }

  this.currentSelection().removeClass('active');
  newSelection.addClass('active');

  newSelection.parents(".column").nextAll().remove();

  if(!this.isSearchboxActive() && newSelection.data('match').type === 'module') {
    this.openColumn();
  }
};

NodeSearcher.prototype.scrollToSelected = function() {
  var currentSelection  = this.currentSelection();

  var visibleBottom  = this.currentColumn.find('ul').scrollTop() + this.currentColumn.find('ul').innerHeight() - 20;
  var selectedBottom = this.currentColumn.find('ul').scrollTop() + currentSelection.position().top + currentSelection.height();

  if(visibleBottom < selectedBottom) {
    this.currentColumn.find('ul').animate({scrollTop: selectedBottom - this.currentColumn.find('ul').innerHeight() + 30 }, config.nodeSearcher.scrollAnimationTime);
  }

  var visibleTop  = this.currentColumn.find('ul').scrollTop();
  var selectedTop = this.currentColumn.find('ul').scrollTop() + currentSelection.position().top;
  if(visibleTop > selectedTop) {
    this.currentColumn.find('ul').animate({scrollTop: selectedTop - 10}, config.nodeSearcher.scrollAnimationTime);
  }
};

NodeSearcher.prototype.onKeyDown = function(event) {
  var currentSelection, nextSelection;

  currentSelection  = this.currentSelection();
  nextSelection = currentSelection.next();

  if(this.isSearchboxActive()) {
    nextSelection = this.firstColumn.find('li:first-child');
  }

  if(nextSelection.length > 0) {
    this.select(nextSelection);
    this.scrollToSelected();
  }

  event.preventDefault();
  event.stopPropagation();
};

NodeSearcher.prototype.onKeyUp = function(event) {
  var currentSelection, nextSelection, shouldSelectSearchbox;
  currentSelection  = this.currentSelection();
  nextSelection = currentSelection.prev();

  shouldSelectSearchbox = this.currentColumn.is(this.firstColumn) && nextSelection.length === 0;

  if(shouldSelectSearchbox) {
    nextSelection = this.searchrow;
  }

  if(nextSelection.length > 0) {
    this.select(nextSelection);
    this.scrollToSelected();
  }

  event.preventDefault();
  event.stopPropagation();
};

NodeSearcher.prototype.onBackspace = function(event) {
  var val = this.searchbox.val();
  var expr = this.expression();

  if(val === "") {
    this.setExpression(expr.slice(0, expr.length-1));
    event.preventDefault();
  }

  event.stopPropagation();
};

NodeSearcher.prototype.isSearchboxActive = function() {
  return this.searchrow.hasClass('active');
};

NodeSearcher.prototype.onKeyLeft = function(event) {
  if(!this.isSearchboxActive()) {
    if(!this.currentColumn.is(this.firstColumn)){
      this.currentColumn.nextAll().remove();
      this.selectColumn(this.currentColumn.prev());
    }
    event.preventDefault();
  }
  event.stopPropagation();
};

NodeSearcher.prototype.onKeyRight = function(event) {
  var next;
  if(!this.isSearchboxActive()) {
    next = this.currentColumn.next();
    if(next.length > 0) {
      this.select(next.find("li:first-child"));
    }
    event.preventDefault();
  }
  event.stopPropagation();
};

NodeSearcher.prototype.performSearch = function() {
  var results;
  if(this.prefix === "" && this.searchbox.val() === "") {
    this.firstColumn.removeClass('types');
  } else {
    this.firstColumn.addClass('types');
  }

  results = doSearch(this.expression());

  this.searchrow.addClass('active');
  this.displaySearchResults(results);
};


NodeSearcher.prototype.onInput = function() {
  var query = this.searchbox.val();
  if(shouldSplit(query)) {
    this.appendExpression(query);
  } else {
    this.performSearch();
  }
};

NodeSearcher.prototype.onEnter = function(ev) {
  var current, data;

  if(this.searchrow.hasClass('active')) {
    this.createNode(this.prefix + this.searchbox.val());
  } else {
    current = this.currentSelection();
    data    = current.data('match');

    if(data.type === 'module') {
      this.appendExpression(data.fullName + ".");
    } else {
      this.appendExpression(data.fullName);
      this.createNode();
    }
  }

  ev.preventDefault();
};

NodeSearcher.prototype.onEsc = function(ev) {
  ev.preventDefault();
};

NodeSearcher.prototype.onKeyHome = function(ev) {
  var _this = this;
  var ul = this.currentColumn.find("ul");
  if(!this.searchrow.hasClass('active')) {
    this.currentColumn.find("ul").animate({scrollTop: 0}, config.nodeSearcher.scrollAnimationTime, function() {
      _this.selectFirstVisible(ul);
    });
    ev.preventDefault();
  }
};

NodeSearcher.prototype.onKeyEnd = function(ev) {
  var _this = this;
  var ul = this.currentColumn.find("ul");
  if(!this.searchrow.hasClass('active')) {
    ul.animate({scrollTop: ul.prop('scrollHeight')}, config.nodeSearcher.scrollAnimationTime, function() {
      _this.selectLastVisible(ul);
    });
    ev.preventDefault();
  }
};

NodeSearcher.prototype.selectFirstVisible = function(ul){
  var firstVisible = _(ul.children()).find(function(el) { return $(el).position().top >= 0; });
  if(firstVisible) {
    this.select($(firstVisible));
  }
};

NodeSearcher.prototype.selectLastVisible = function(ul){
  var lastVisible = _(ul.children().get().reverse()).find(function(el) { return $(el).position().top + $(el).height() <= ul.height(); });
  if(lastVisible) {
    this.select($(lastVisible));
  }
};

NodeSearcher.prototype.onKeyPgUp = function(ev) {
  var _this = this;
  var ul = this.currentColumn.find("ul");
  var targetScroll;

  if(!this.searchrow.hasClass('active')) {
    targetScroll = ul.prop('scrollTop') - ul.height() + 50;
    ul.animate({scrollTop: targetScroll}, config.nodeSearcher.scrollAnimationTime, function() {
      _this.selectLastVisible(ul);
    });
    ev.preventDefault();
  }
};

NodeSearcher.prototype.onKeyPgDn = function(ev) {
  var _this = this;
  var ul = this.currentColumn.find("ul");
  var targetScroll;

  if(!this.searchrow.hasClass('active')) {
    targetScroll = ul.prop('scrollTop') + ul.height() - 50;
    ul.animate({scrollTop: targetScroll}, config.nodeSearcher.scrollAnimationTime, function() {
      _this.selectFirstVisible(ul);
    });
    ev.preventDefault();
  }
};

NodeSearcher.prototype.onTab = function(ev) {
  var current, data;

  if(!this.searchrow.hasClass('active')) {
    current = this.currentSelection();
    data    = current.data('match');

    if(data.type === 'module') {
      this.appendExpression(data.fullName + ".");
    } else {
      this.appendExpression(data.fullName );
    }
  }

  ev.preventDefault();
};

NodeSearcher.prototype.initSearchbox = function() {
  var _this = this;

  var firstColumn = $('<div/>').addClass('column').addClass('current').addClass('first-column');
  this.searchrow = $('<div class="item active query"><span class="query-ns"></span><input autofocus="on" type="text" class="query"/></div>');
  firstColumn.items = $('<ul/>');
  firstColumn.append(this.searchrow);
  firstColumn.append(firstColumn.items);

  this.searchbox = this.searchrow.find('input.query');
  this.searchns  = this.searchrow.find('span.query-ns');

  this.el.append(firstColumn);
  this.currentColumn = firstColumn;
  this.firstColumn = firstColumn;

  this.updatePrefixWidth();

  this.searchbox.on('input', function() {
    _this.onInput();
  });
  this.searchbox.on('blur', function() {
    _this.searchbox.focus();
  });

  this.searchbox.on('keydown', function(ev) {
    switch(ev.keyCode){
      case 9: { //tab
        _this.onTab(ev);
        break;
      }
      case 27: { //esc
        _this.onEsc(ev);
        break;
      }
      case 8: {
        _this.onBackspace(ev);
        break;
      }
      case 38: { //up
        _this.onKeyUp(ev);
        break;
      }
      case 40: { // down
        _this.onKeyDown(ev);
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
      case 33: { //pgup
        _this.onKeyPgUp(ev);
        break;
      }
      case 34: { //pgdwn
        _this.onKeyPgDn(ev);
        break;
      }
      case 36: { //home
        _this.onKeyHome(ev);
        break;
      }
      case 35: { //end
        _this.onKeyEnd(ev);
        break;
      }
    }
  });


  var x, y;
  this.el.on("mousemove", ".column .item", function(ev) {
    if(x !== ev.pageX || y !== ev.pageY) {
      x = ev.pageX;
      y = ev.pageY;
      _this.select($(ev.currentTarget));
    }
  });
  this.el.on("click", ".column ul li.result", function(ev) {
    _this.select($(ev.currentTarget));
    _this.onEnter(ev);
  });
};

NodeSearcher.prototype.createNode = function() {
  alert(this.expression());
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
  this.displayTree(doSearchTree(this.currentSelection().data('match').fullName), column.items);
};

NodeSearcher.prototype.updatePrefixWidth = function() {
  this.prefixWidth = 90+this.searchns.width();
  this.firstColumn.find('.ns').css({idth: this.prefixWidth});
};

NodeSearcher.prototype.displaySearchResults = function(results) {
  this.firstColumn.nextAll().remove();
  this.currentColumn = this.firstColumn;
  this.displayResults(results, this.firstColumn.items);
};

NodeSearcher.prototype.displayResults = function(results, ul) {
  var _this = this;
  ul.find('li.result').remove();

  _(results).each(function(item) {
    var li = $('<li/>').addClass('result').addClass('item');
    var ns = $("<span/>").addClass('ns').text(item.module).css({minWidth: _this.prefixWidth});
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
    var li = $('<li/>').addClass('result').addClass('item');
    var name = $("<span/>").addClass('fname').text(item.name);

    li.data('match', item);
    li.addClass(item.type);

    ul.append(li);
    li.append(name);
  });
};

module.exports = NodeSearcher;