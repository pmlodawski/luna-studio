var global = window;

var h$GHCJSi = { out: function(dat) {}
		 , msg: function(msgType, msgPayload) {}
		 , current: null
		 , loadedSymbols: {}
		 , done: function(thread) {
		   h$GHCJSi.current = null;
		 }
};

window.h$GHCJSi = h$GHCJSi;
