"use strict";

// module UI

exports.registerEventHandler = function(handler) {
  return function() {
    window.eventHandler = function(msg){handler(msg)()};

		// a little ghetto
		__  = function(msg) {handler(msg)();}
		__M = function(msg) {handler("setP main.main_body." + msg)();}
		__D = function(msg) {handler("setP disp." + msg)();}
		__T = function(msg) {handler("setT " + msg)();}
  }
};

exports.registerKeyHandler = function(handler) {
  return function(){
		document.onkeypress = function(event) {
			var code = event.which || event.keyCode
			var chr = String.fromCharCode(code);
			var cmd = handler(chr)();
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
			if (isFullScreen) {
				$('#exitFullScreen').removeClass('hide');
				$('#enterFullScreen').addClass('hide');
			} else {
				$('#exitFullScreen').addClass('hide');
				$('#enterFullScreen').removeClass('hide');
			}
			handler('fullWindow')();
		};

		$(document).on('webkitfullscreenchange mozfullscreenchange fullscreenchange MSFullscreenChange', fsHandler);


		var resizeHandler = function(){
			handler('initLayout')();
		}

		$(window).resize(resizeHandler);

		$("#menu-icon").hover(function(){$("#menuContainer").fadeIn("slow");$("#menu-icon").fadeOut("slow");},function(){});
		$("#menuContainer").hover(function(){},function(){$("#menuContainer").fadeOut("slow");$("#menu-icon").fadeIn("slow");});

		var resChangeHandler =
				function (){
					handler("setKernelDim " + this.value)();
				};

		$("#resolutionSel").on('change', resChangeHandler);
  }
};
