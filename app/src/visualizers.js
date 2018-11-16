"use strict";
var knownFrames = {};
var framesToLoad = [];
var retriesLimit = 100;

var sendToFrame = function (id, data) {
  document.getElementsByName(id)[0].contentWindow.postMessage(data, "*");
};

var flushAll = function (id) {
  if(knownFrames[id]) {
    var queue = knownFrames[id].data;
    delete knownFrames[id];
    if (queue) queue.forEach(function (data) { sendToFrame(id, data); });
  }
};

var askIfLoaded = function (id) {
  if(knownFrames[id] && knownFrames[id].data!=[]) {
    var frame = document.getElementsByName(id)[0];
    if (frame)
      frame.contentWindow.postMessage({ping: true, id: id}, "*");
    if(knownFrames[id].retries < retriesLimit) {
      knownFrames[id].retries++;
      setTimeout(function() {askIfLoaded(id)}, 100);
    } else knownFrames[id].retries=0;
  }
};

window.addEventListener("message", function(evt) {
  if(evt.data.ping)
    flushAll(evt.data.id);
});


var register = function (id) {
  knownFrames[id] = {data: [], retries: 0};
  askIfLoaded(id);
};

var send = function (id, data) {
  if (knownFrames[id] && knownFrames[id].data) {
    knownFrames[id].data.push(data);
    askIfLoaded(id);
  } else sendToFrame(id, data);
};

var sendData = function (id, type, data) {
  send(id, { event: "data", type: type, data: data });
};

var sendInternalData = function (id, data) {
  send(id, { event: "internalData", data: data });
};

var sendDatapoint = function (id, data) {
  send(id, { event: "datapoint", data: data });
};

var notifyStreamRestart = function (id, type, backup) {
  send(id, { event: "restart", type: type, data: backup });
};

module.exports = {
  sendData: sendData,
  register: register,
  notifyStreamRestart: notifyStreamRestart,
  sendDatapoint: sendDatapoint,
  sendInternalData: sendInternalData
};
