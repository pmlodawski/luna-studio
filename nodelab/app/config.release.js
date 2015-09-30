module.exports = {
  backend: true,
  backendAddress: (window.backendAddress || "wss://demo.nodelab.io/backend"),
  logging:         false,
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
