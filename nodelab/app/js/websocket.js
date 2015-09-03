"use strict";

var removeFromArray = function (array, elt) {
  var index = array.indexOf(elt);
  array.splice(index, 1);
};

var mockSocket = function () {
  var listeners = [];

  return {
    addEventListener: function (type, lst) {
      if (type === "message")
        listeners.push(lst);
    },
    removeEventListener: function (type, lst) {
      if (type === "message")
        removeFromArray(listeners, lst);
    },
    send: function (data) {
      var decoded = atob(data);
      var replaced = decoded.replace("request", "fakeres");
      var fakeResponse = { data: btoa(replaced) };
      listeners.forEach(function (listener) {
        listener.call(null, fakeResponse);
      });
    }
  };
};

module.exports = function () {
  var connection = mockSocket();

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
      connection.removeEventListener("message", listener);
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
