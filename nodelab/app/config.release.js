"use strict";

function defaultBackend() {
    var l = window.location;
    return ((l.protocol === "https:") ? "wss://" : "ws://") + l.hostname + (((l.port !== 80) && (l.port !== 443)) ? ":" + l.port : "") + "/ws";
}

module.exports = {
  backend: true,
  backendAddress: (window.backendAddress || defaultBackend()),
  logging:         false,
  exportState:     false,
  backgroundColor: 0x1a1a1a,
  fontSize:        0.45,
  nodeSearcher: {
    scrollAnimationTime: 100,
    scrollbarOptions: {
      alwaysExpandScrollbar: true,
      alwaysShowScrollbar: 2,
      updateOnSelectorChange: "ul li",
      updateOnContentResize: true
    }
  }
};
