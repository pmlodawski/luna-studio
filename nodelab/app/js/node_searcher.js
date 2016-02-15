"use strict";

var $$     = require('./common')
  , config = require('./config')
;


function highlightText(name, highlight) {
  highlight.push({start: name.length, length: 0});

  return _(highlight).foldl(function (acc, el) {
    if (acc.pos < el.start) {
      acc.elems.push(name.substring(acc.pos, el.start));
    }

    var part = name.substring(el.start, el.start+el.length);
    if (part.length > 0) {
      acc.elems.push($("<em/>").text(part));
    }

    acc.pos = el.start + el.length;

    return acc;
  }, {pos: 0, elems: []});
}

function shouldSplit(query) {
  return (query.indexOf('.') > -1 || query.indexOf(' ') > -1);
}

function splitExpression(expr, command) {
  var prefix, query, idx;
  if (command) {
    idx = Math.max(expr.lastIndexOf('.'), expr.indexOf(' '));
  } else {
    idx = Math.max(expr.lastIndexOf('.'), expr.lastIndexOf(' '));
  }
  prefix = expr.substring(0, idx+1);
  query  = expr.substring(idx+1);
  return {prefix: prefix, query: query};
}

function NodeSearcher() {
  this.el = $('<div/>').addClass('node-searcher');
}

NodeSearcher.prototype.init = function (nodeId, command) {
  var self = this;
  this.command = command;
  if(command) {
    this.actionTree   = 'treeCmd';
    this.actionCreate = 'createCmd';
    this.actionQuery  = 'queryCmd';
    this.el.addClass('command');
  } else {
    this.actionTree   = 'tree';
    this.actionCreate = 'create';
    this.actionQuery  = 'query';
  }
  this.prefix = "";
  this.nodeId = nodeId;
  this.initSearchbox();
  this.setExpression("");
  this.searchbox.focus();
  setTimeout(function (){ self.searchbox.focus();}, 30);
};

NodeSearcher.prototype.initSearchbox = function () {
  var _this = this;

  var firstColumn = $('<div/>').addClass('column').addClass('current').addClass('first-column');
  this.searchrow = $('<div class="item active query"><span class="query-ns"></span><input autofocus="on" type="text" class="query"/></div>');
  firstColumn.items = $('<ul/>');
  firstColumn.itemsDiv = $('<div class="ul-container"/>');
  firstColumn.append(this.searchrow);
  firstColumn.itemsDiv.append(firstColumn.items);
  firstColumn.append(firstColumn.itemsDiv);

  this.searchbox = this.searchrow.find('input.query');
  this.searchns  = this.searchrow.find('span.query-ns');

  this.el.append(firstColumn);
  this.currentColumn = firstColumn;
  this.firstColumn = firstColumn;

  this.updatePrefixWidth();

  firstColumn.itemsDiv.mCustomScrollbar(config.nodeSearcher.scrollbarOptions);

  this.searchbox.on('input', function () {
    _this.onInput();
  });
  this.searchbox.on('blur', function () {
    _this.searchbox.focus();
  });

  this.searchbox.on('keydown', function (ev) {
    switch(ev.keyCode){
      case 9:  /* tab         */ _this.onTab(ev);       break;
      case 27: /* esc         */ _this.onEsc(ev);       break;
      case 8:  /* backspace   */ _this.onBackspace(ev); break;
      case 38: /* arrow-up    */ _this.onKeyUp(ev);     break;
      case 40: /* arrow-down  */ _this.onKeyDown(ev);   break;
      case 37: /* arrow-left  */ _this.onKeyLeft(ev);   break;
      case 39: /* arrow-right */ _this.onKeyRight(ev);  break;
      case 13: /* enter       */ _this.onEnter(ev);     break;
      case 33: /* page-up     */ _this.onKeyPgUp(ev);   break;
      case 34: /* page-down   */ _this.onKeyPgDn(ev);   break;
      case 36: /* home        */ _this.onKeyHome(ev);   break;
      case 35: /* end         */ _this.onKeyEnd(ev);    break;
    }
    ev.stopPropagation();
  });

  this.searchbox.on('keypress', function (ev) { // prevent Haskell handlers
    ev.stopPropagation();
  });

  this.searchbox.on('keyup', function (ev) { // prevent Haskell handlers
    ev.stopPropagation();
  });

  var x, y;
  this.el.on("mousemove", ".column .item", function (ev) {
    if (x !== ev.pageX || y !== ev.pageY) {
      x = ev.pageX;
      y = ev.pageY;
      _this.select($(ev.currentTarget));
    }
  });
  this.el.on("click", ".column ul li.result", function (ev) {
    _this.select($(ev.currentTarget));
    _this.onEnter(ev);
  });
};

NodeSearcher.prototype.expression = function () {
  return this.prefix + this.searchbox.val();
};

NodeSearcher.prototype.setExpression = function (expr) {
  var split = splitExpression(expr, this.command);
  this.prefix = split.prefix;
  this.searchbox.val(split.query);
  this.searchns.text(split.prefix);
  this.updatePrefixWidth();
  this.performSearch();
};

NodeSearcher.prototype.appendExpression = function (expr) {
  this.setExpression(this.prefix + expr);
};


NodeSearcher.prototype.performSearch = function () {
  var ev;
  if (this.prefix === "" && this.searchbox.val() === "") {
    this.firstColumn.removeClass('types');
    this.clearResults();
    ev = new CustomEvent('ns_event', {
      detail: {
        action: this.actionTree,
        expression: ""
      }
    });
    window.dispatchEvent(ev);
  } else {
    this.firstColumn.addClass('types');
    ev = new CustomEvent('ns_event', {
      detail: {
        action: this.actionQuery,
        expression: this.expression()
      }
    });
    window.dispatchEvent(ev);
  }

};

NodeSearcher.prototype.returnSearchResult = function (results) {
  this.searchrow.addClass('active');
  this.displaySearchResults(results);
};

NodeSearcher.prototype.currentSelection = function () {
  return this.currentColumn.find('.item.active');
};

NodeSearcher.prototype.select = function (newSelection) {
  if (newSelection.length === 0) return;
  if (this.currentSelection().is(newSelection)) return;

  var inSameColumn = this.currentSelection().parents('.column').is(newSelection.parents('.column'));

  if (!inSameColumn) {
    this.selectColumn(newSelection.parents('.column'));
  }

  this.currentSelection().removeClass('active');
  newSelection.addClass('active');

  newSelection.parents(".column").nextAll().remove();

  if (!this.isSearchboxActive() && newSelection.data('match').type === 'module') {
    this.openColumn();
  }
};

NodeSearcher.prototype.scrollToSelected = function () {
  var currentSelection  = this.currentSelection();

  var selectedBottom = currentSelection.position().top + this.currentColumn.find('.mCSB_container').position().top + this.currentSelection().height();

  if (selectedBottom > this.currentColumn.find(".ul-container").height()) {
    this.currentColumn.find('.ul-container').mCustomScrollbar("scrollTo", currentSelection.position().top - this.currentColumn.find('.ul-container').innerHeight() + 30, {scrollInertia: config.nodeSearcher.scrollAnimationTime});
  }

  var selectedTop = currentSelection.position().top + this.currentColumn.find('.mCSB_container').position().top;
  if (selectedTop < 0) {
    this.currentColumn.find('.ul-container').mCustomScrollbar("scrollTo", Math.max(0, currentSelection.position().top - currentSelection.height()), {scrollInertia: config.nodeSearcher.scrollAnimationTime});
  }
};

NodeSearcher.prototype.isSearchboxActive = function () {
  return this.searchrow.hasClass('active');
};

NodeSearcher.prototype.createNode = function () {
  var ev = new CustomEvent('ns_event', {
    detail: {
      action: this.actionCreate,
      expression: this.expression(),
      node: this.nodeId
    }
  });
  this.destroy();
  window.dispatchEvent(ev);
};

NodeSearcher.prototype.selectColumn = function (column) {
  this.currentColumn.removeClass('current');
  this.currentColumn = column;
  this.currentColumn.addClass('current');
  this.currentColumn.nextAll().find("li").removeClass("active");
  if (this.currentColumn.find("li.active").length === 0) {
    this.currentColumn.find("li:first-child").addClass("active");
  }
};

NodeSearcher.prototype.openColumn = function () {
  var column = $('<div/>').addClass('column');
  column.items = $('<ul/>');
  column.itemsDiv = $('<div class="ul-container"/>');
  column.itemsDiv.append(column.items);
  column.append(column.itemsDiv);

  this.el.append(column);
  column.itemsDiv.mCustomScrollbar(config.nodeSearcher.scrollbarOptions);

  column.data('items', column.items);

  var ev = new CustomEvent('ns_event', {
    detail: {
      action: this.actionTree,
      expression: this.currentSelection().data('match').fullname
    }
  });
  window.dispatchEvent(ev);
};

NodeSearcher.prototype.updatePrefixWidth = function () {
  this.prefixWidth = 90+this.searchns.width();
  this.firstColumn.find('.ns').css({idth: this.prefixWidth});
};


NodeSearcher.prototype.clearResults = function () {
  this.firstColumn.nextAll().remove();
  this.currentColumn = this.firstColumn;
  this.firstColumn.find('li.result').remove();
  this.searchrow.addClass('active');
};

NodeSearcher.prototype.addResult = function (prefix, name, fullname, highlight, type) {
  var ul = this.firstColumn.find('ul');

  var li = $('<li/>').addClass('result').addClass('item');
  var ns = $("<span/>").addClass('ns').text(prefix).css({minWidth: this.prefixWidth});
  var namespan = $("<span/>").addClass('fname');

  li.data('match', {module: prefix, name: name, fullname: fullname, highlight: highlight, type: type});
  li.addClass(type);

  ul.append(li);
  li.append(ns);
  li.append(namespan);

  _(highlightText(name, highlight).elems).each(function (part){
    namespan.append(part);
  });
};



// -> HS
NodeSearcher.prototype.displaySearchResults = function (results) {
  this.firstColumn.nextAll().remove();
  this.currentColumn = this.firstColumn;
  this.displayResults(results, this.firstColumn.items);
};

// -> HS
NodeSearcher.prototype.displayResults = function (results, ul) {
  var _this = this;
  ul.find('li.result').remove();

  _(results).each(function (item) { // -> inside stays in JS
    var li = $('<li/>').addClass('result').addClass('item');
    var ns = $("<span/>").addClass('ns').text(item.module).css({minWidth: _this.prefixWidth});
    var name = $("<span/>").addClass('fname');

    li.data('match', item);
    li.addClass(item.type);

    ul.append(li);
    li.append(ns);
    li.append(name);

    _(highlightText(item.name, item.highlight).elems).each(function (part){
      name.append(part);
    });
  });
};



NodeSearcher.prototype.addTreeResult = function (prefix, name, fullname, type) {
  var ul = this.el.find('.column:last-child ul');

  var li = $('<li/>').addClass('result').addClass('item');
  var namespan = $("<span/>").addClass('fname').text(name);
  li.append(namespan);

  li.data('match', {module: prefix, name: name, fullname: fullname, type: type});
  li.addClass(type);

  ul.append(li);
};

// input event handlers

NodeSearcher.prototype.onInput = function () {
  var query = this.searchbox.val();
  if (shouldSplit(query)) {
    this.appendExpression(query);
  } else {
    this.performSearch();
  }
};

NodeSearcher.prototype.onBackspace = function (event) {
  var val = this.searchbox.val();
  var expr = this.expression();

  if (val === "") {
    this.setExpression(expr.slice(0, expr.length-1));
    event.preventDefault();
  }

  event.stopPropagation();
};

NodeSearcher.prototype.onEnter = function (ev) {
  var current, data;

  if (this.searchrow.hasClass('active')) {
    this.createNode(this.prefix + this.searchbox.val());
  } else {
    current = this.currentSelection();
    data    = current.data('match');

    if (data.type === 'module') {
      this.appendExpression(data.fullname + ".");
    } else {
      this.appendExpression(data.fullname);
      this.createNode();
    }
  }

  ev.preventDefault();
};

NodeSearcher.prototype.onTab = function (ev) {
  var current, data;
  ev.preventDefault();

  if (!this.searchrow.hasClass('active')) {
    current = this.currentSelection();
  } else {
    current = this.currentColumn.find("li:first-child");
  }

  if (current.length === 0) return;

  data    = current.data('match');

  if (data.type === 'module') {
    this.appendExpression(data.fullname + ".");
  } else {
    this.appendExpression(data.fullname );
  }

};

NodeSearcher.prototype.onEsc = function (ev) {
  // TODO: Close searcher and send 'search-cancelled' event
  ev.preventDefault();
  this.destroy();
};

NodeSearcher.prototype.onKeyDown = function (event) {
  var currentSelection, nextSelection;

  currentSelection  = this.currentSelection();
  nextSelection = currentSelection.next();

  if (this.isSearchboxActive()) {
    nextSelection = this.firstColumn.find('li:first-child');
  }

  if (nextSelection.length > 0) {
    this.select(nextSelection);
    this.scrollToSelected();
  }

  event.preventDefault();
  event.stopPropagation();
};

NodeSearcher.prototype.onKeyUp = function (event) {
  var currentSelection, nextSelection, shouldSelectSearchbox;
  currentSelection  = this.currentSelection();
  nextSelection = currentSelection.prev();

  shouldSelectSearchbox = this.currentColumn.is(this.firstColumn) && nextSelection.length === 0;

  if (shouldSelectSearchbox) {
    nextSelection = this.searchrow;
  }

  if (nextSelection.length > 0) {
    this.select(nextSelection);
    this.scrollToSelected();
  }

  event.preventDefault();
  event.stopPropagation();
};

NodeSearcher.prototype.onKeyLeft = function (event) {
  if (!this.isSearchboxActive()) {
    if (!this.currentColumn.is(this.firstColumn)){
      this.currentColumn.nextAll().remove();
      this.selectColumn(this.currentColumn.prev());
    }
    event.preventDefault();
  }
  event.stopPropagation();
};

NodeSearcher.prototype.onKeyRight = function (event) {
  var next;
  if (!this.isSearchboxActive()) {
    next = this.currentColumn.next();
    if (next.length > 0) {
      this.select(next.find("li:first-child"));
    }
    event.preventDefault();
  }
  event.stopPropagation();
};

NodeSearcher.prototype.onKeyHome = function (ev) {
  if (!this.searchrow.hasClass('active')) {
    this.select(this.currentColumn.find('li:first-child'));
    this.currentColumn.find('.ul-container').mCustomScrollbar("scrollTo", 'top', {scrollInertia: config.nodeSearcher.scrollAnimationTime});
    ev.preventDefault();
  }
};

NodeSearcher.prototype.onKeyEnd = function (ev) {
  if (!this.searchrow.hasClass('active')) {
    this.select(this.currentColumn.find('li:last-child'));
    this.currentColumn.find('.ul-container').mCustomScrollbar("scrollTo", 'bottom', {scrollInertia: config.nodeSearcher.scrollAnimationTime});
    ev.preventDefault();
  }
};

NodeSearcher.prototype.onKeyPgUp = function (ev) {
  var targetScroll = this.currentSelection().position().top - this.currentColumn.find(".ul-container").height();

  if (!this.searchrow.hasClass('active')) {
    var lastVisible = _(this.currentColumn.find('li').get()).find(function (el) { return $(el).position().top >= targetScroll;});
    if (lastVisible) {
      this.select($(lastVisible));
    }
    this.scrollToSelected();
    ev.preventDefault();
  }
};

NodeSearcher.prototype.onKeyPgDn = function (ev) {
  var targetScroll = this.currentSelection().position().top + this.currentColumn.find(".ul-container").height();

  if (!this.searchrow.hasClass('active')) {
    var lastVisible = _(this.currentColumn.find('li').get().reverse()).find(function (el) { return $(el).position().top + $(el).height() <= targetScroll;});
    if (lastVisible) {
      this.select($(lastVisible));
    }
    this.scrollToSelected();
    ev.preventDefault();
  }
};

NodeSearcher.prototype.destroy = function () {
  this.el.remove();
  $('#canvas2d').focus();
  $$.node_searcher = undefined;
};

module.exports = NodeSearcher;
