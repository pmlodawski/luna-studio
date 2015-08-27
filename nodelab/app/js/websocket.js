"use strict";

module.exports = function () {
  var connection = {
    addEventListener: function () {}
  };

  var listeners = {
    onOpen: [],
    onMessage: []
  };

  var attachListeners = function () {
    listeners.onOpen.forEach(function (listener) {
      connection.addEventListener("open", listener);
    });
    listeners.onMessage.forEach(function (listener) {
      connection.addEventListener("message", listener);
    });
  };

  var removeFromArray = function (array, elt) {
    var index = array.indexOf(elt);
    array.splice(index, 1);
  };

  return {
    onOpen: function (listener) {
      listeners.onOpen.push(listener);
      connection.addEventListener("open", listener);
    },
    unOnOpen: function (listener) {
      removeFromArray(listeners.onOpen, listener);
      connection.removeEventListener("open", listener);
    },
    onMessage: function (listener) {
      listeners.onMessage.push(listener);
      connection.addEventListener("message", listener);
    },
    unOnMessage: function (listener) {
      removeFromArray(listeners.onMessage, listener);
      connection.removeEventListener("open", listener);
    },
    connect: function (addr) {
      connection = new WebSocket(addr);
      attachListeners();
    },
    send: function (data) {
      connection.send(data);
    }
  };
};
