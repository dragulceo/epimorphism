"use strict";

// module UI

exports.registerEventHandler = function(handler) {
  return function() {
    window.eventHandler = function(msg){handler(msg)()};
  }
};

exports.registerKeyHandler = function(handler) {
  return function(){
		document.onkeydown = function(event) {
			var code = String.fromCharCode(event.keyCode);
			var cmd = handler(code)();
// fix me
//			if(cmd != "null")
//				event.preventDefault();
			window.eventHandler(cmd);
		};
	};
};


exports.addGlobalEventListeners = function(handler) {
  return function() {
		var fsHandler = function(event){
			var isFullScreen = document.fullScreen ||
          document.mozFullScreen ||
          document.webkitIsFullScreen;

			if (!isFullScreen) {
				handler("fullWindow")();
			}
		};

		$(document).on('webkitfullscreenchange mozfullscreenchange fullscreenchange MSFullscreenChange', fsHandler);
  }
};
