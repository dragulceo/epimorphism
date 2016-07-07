"use strict";

// module UI

exports.registerEventHandler = function(handler) {
  return function() {
    window.eventHandler = function(msg){handler(msg)()};
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

				handler("fullWindow")();
			}
		};

		$(document).on('webkitfullscreenchange mozfullscreenchange fullscreenchange MSFullscreenChange', fsHandler);


		var iconOverHandler =
				function(event){
					$('#menu-icon').addClass('hide');
					$('#menuContainer').removeClass('hide');
					$('#menuContainer').addClass('fadeIn');
				};

		$("#menu-icon").on('mouseover', iconOverHandler);


		var menuExitHandler =
				function(event){
					if($.inArray(event.target.tagName.toLowerCase(), ["select", "option", "input", "a", "span"]) == -1){
						$('#menu-icon').removeClass('hide');
						$('#menuContainer').addClass('hide');
						$('#menuContainer').removeClass('fadeIn');
					}
				};

		$("#menuContainer").on('mouseleave', menuExitHandler);


		var resChangeHandler =
				function (){
					handler("setKernelDim " + this.value)();
				};

		$("#resolutionSel").on('change', resChangeHandler);
  }
};
