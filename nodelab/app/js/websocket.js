"use strict";

module.exports = function () {
  var connection = {};

  var listeners = {
    onOpen: undefined,
    onMessage: undefined
  };

  var attachListeners = function () {
    connection.onopen = listeners.onOpen;
    connection.onmessage = listeners.onOmessage;
  };

  return {
    onOpen: function (listener) {
      listeners.onOpen = listener;
      connection.onopen = listener;
    },
    unOnOpen: function () {
      listeners.onOpen = undefined;
      connection.onmessage = undefined;
    },
    onMessage: function (listener) {
      listeners.onMessage = listener;
      connection.onmessage = listener;
    },
    unOnMessage: function () {
      listeners.onMessage = undefined;
      connection.onmessage = undefined;
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
