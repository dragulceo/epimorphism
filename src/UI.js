"use strict";

// module UI

exports.registerEventHandler = function(handler) {
  return function() {
    window.eventHandler = function(msg){handler(msg)()};
  }
};


exports.requestFullScreen = function(id) {
  return function() {
    var elem = document.getElementById(id);
    if (elem.requestFullscreen) {
      elem.requestFullscreen();
    } else if (elem.msRequestFullscreen) {
      elem.msRequestFullscreen();
    } else if (elem.mozRequestFullScreen) {
      elem.mozRequestFullScreen();
    } else if (elem.webkitRequestFullscreen) {
      elem.webkitRequestFullscreen();
    }
	};
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
